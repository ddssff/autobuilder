{-# LANGUAGE OverloadedStrings #-}
module Debian.AutoBuilder.Types.Fingerprint
    ( Fingerprint(..)
    , readUpstreamFingerprint
    , DownstreamFingerprint
    , packageFingerprint
    , modernizeMethod
    , showFingerprint
    , dependencyChanges
    , showDependencies
    , showDependencies'
    , targetFingerprint
    , buildDecision
    ) where

import Control.Applicative.Error (maybeRead)
import Data.Char (isSpace)
import Data.Generics (everywhere, mkT)
import Data.List as List (intercalate, intersperse, map, nub, partition)
import qualified Data.Map as Map
import Data.Maybe(isNothing, listToMaybe, mapMaybe)
import Data.Set as Set (Set, toList, toAscList, difference, empty, fromList, map, filter)
import Data.Text (unpack)
import Debian.AutoBuilder.Types.Buildable (Target(tgt, cleanSource), Buildable(download, debianSourceTree), targetRelaxed, relaxDepends)
import qualified Debian.AutoBuilder.Types.CacheRec as P
import qualified Debian.AutoBuilder.Types.Download as T
import qualified Debian.AutoBuilder.Types.ParamRec as P
import qualified Debian.AutoBuilder.Types.Packages as P
import Debian.Changes (logVersion)
import Debian.Control (fieldValue, debianSourcePackageName, debianBinaryParagraphs)
import qualified Debian.Control.String as S
import qualified Debian.GenBuildDeps as G
import Debian.Pretty (pretty)
import Debian.Relation (Relation(Rel), BinPkgName(..))
import Debian.Repo.Dependencies (prettySimpleRelation, readSimpleRelation, showSimpleRelation)
import Debian.Repo.SourceTree (HasChangeLog(entry), SourcePackageStatus(..), BuildDecision(..))
import Debian.Repo.PackageID (PackageID(PackageID, packageName, packageVersion))
import Debian.Repo.PackageIndex (SourcePackage(sourceParagraph, sourcePackageID), BinaryPackage(packageID))
import Debian.Version (DebianVersion, parseDebianVersion, prettyDebianVersion)
import Extra.Misc(columns)

-- | This type represents a package's fingerprint, (formerly its
-- revision string,) which includes three pieces of information: how
-- it was retrieved, the version number of the resulting Debian source
-- package, and the names and version numbers of the build
-- dependencies against which it was or is about to be built.
data Fingerprint
    = Fingerprint
        { method :: P.RetrieveMethod
          -- ^ The method which was used to retrieve the source code.
        , retrievedAttributes :: Set P.RetrieveAttribute
          -- ^ Identifying information about the result of the retrieve,
          -- e.g. the debian version number, darcs repository tag, git
          -- commit id.  Replaces upstreamVersion.
        , upstreamVersion :: DebianVersion
          -- ^ The version number in the changelog of the freshly downloaded
          -- package, before any suffix is added by the autobuilder.
        , buildDependencyVersions :: Set (PackageID BinPkgName)
          -- ^ The names and version numbers of the build dependencies which
          -- were present when the package was build.
        }
    deriving (Show, Eq)

data DownstreamFingerprint
    = DownstreamFingerprint
        { upstreamFingerprint :: Fingerprint
        , downstreamVersion :: DebianVersion
          -- ^ This will be the the version field plus the suffix that
          -- was added by the autobuilder.
        }
    deriving Show

readMethod :: String -> Maybe P.RetrieveMethod
readMethod s = maybeRead s

packageFingerprint :: SourcePackage -> Maybe DownstreamFingerprint
packageFingerprint package =
    maybe Nothing readDownstreamFingerprint (fmap (unpack . strip) . S.fieldValue "Fingerprint" . sourceParagraph $ package)
    where
      readDownstreamFingerprint :: String -> Maybe DownstreamFingerprint
      readDownstreamFingerprint s =
          maybe Nothing
                (\ f -> Just $ DownstreamFingerprint { upstreamFingerprint = f
                                                     , downstreamVersion = packageVersion . sourcePackageID $ package })
                (readUpstreamFingerprint s)

readUpstreamFingerprint :: String -> Maybe Fingerprint
readUpstreamFingerprint s =
          case reads s :: [(String, String)] of
            [(method, etc)] ->
                case readMethod method of
                  Nothing -> Nothing
                  Just method' ->
                      let method'' = modernizeMethod method' in
                      -- See if there is a list of RetrieveAttribute - if not use the empty list
                      let (attrs, etc') = case reads etc :: [([P.RetrieveAttribute], String)] of
                                            [(x, etc')] -> (x, etc')
                                            _ -> ([], etc) in
                      case words etc' of
                        (sourceVersion : buildDeps)
                          | not (elem '=' sourceVersion) ->
                              Just $ Fingerprint { method = method''
                                                 , upstreamVersion = parseDebianVersion sourceVersion
                                                 , retrievedAttributes = Set.fromList attrs
                                                 , buildDependencyVersions = fromList (List.map readSimpleRelation buildDeps) }
                        -- Old style fingerprint field - no upstream
                        -- version number after the method.  I think
                        -- at this point we can ignore these.
                        _ -> Nothing
            _ -> Nothing

modernizeMethod :: P.RetrieveMethod -> P.RetrieveMethod
modernizeMethod = everywhere (mkT modernizeMethod1)

modernizeMethod1 :: P.RetrieveMethod -> P.RetrieveMethod
modernizeMethod1 (P.Debianize p) = P.Debianize' p []
modernizeMethod1 x = x

showFingerprint :: Fingerprint -> String
showFingerprint (Fingerprint {method = method, upstreamVersion = sourceVersion, retrievedAttributes = attrs, buildDependencyVersions = versions}) =
    intercalate " " [show (show method),
                     "[" ++ intercalate ", " (List.map show (toAscList attrs)) ++ "]",
                     show (prettyDebianVersion sourceVersion),
                     intercalate " " (List.map showSimpleRelation (toAscList versions))]

showDependencies :: Fingerprint -> [String]
showDependencies (Fingerprint {buildDependencyVersions = deps}) = toAscList $ Set.map showSimpleRelation deps

-- | Show the dependency list without the version numbers.
showDependencies' :: Fingerprint -> [String]
showDependencies' (Fingerprint {buildDependencyVersions = deps}) = toAscList $ Set.map (show . pretty . packageName) deps

dependencyChanges :: Maybe DownstreamFingerprint -> Fingerprint -> String
dependencyChanges old new =
    depChanges changedDeps
    where
      depChanges [] = ""
      depChanges _ = "  * Build dependency changes:" ++ prefix ++ intercalate prefix padded ++ "\n"
      padded = List.map concat . columns . List.map showDepChange $ changedDeps
      changedDeps = Set.toList (Set.difference (buildDependencyVersions new) (buildDependencyVersions new))
      showDepChange newDep =
          case toList (Set.filter (hasName (packageName newDep)) (maybe empty (buildDependencyVersions . upstreamFingerprint) old)) of
            [] -> [" " ++ unBinPkgName (packageName newDep) ++ ": ", "(none)", " -> ", show (prettyDebianVersion (packageVersion newDep))]
            (oldDep : _) -> [" " ++ unBinPkgName (packageName newDep) ++ ": ", show (prettyDebianVersion (packageVersion oldDep)), " -> ", show (prettyDebianVersion (packageVersion newDep))]
      hasName name = ((== name) . packageName)
      prefix = "\n    "

targetFingerprint :: Target -> [BinaryPackage] -> Fingerprint
targetFingerprint target buildDependencySolution =
    Fingerprint { method = sourceRevision
                , upstreamVersion = sourceVersion
                , retrievedAttributes = T.attrs $ download $ tgt target
                , buildDependencyVersions = sourceDependencies }
    where
      -- Compute the Revision: string for the source tree.  This
      -- string will appear in the .dsc file for the package, and will
      -- then be copied into the Sources.gz file of the distribution.
      -- For a TLA target this is the current revision name, by
      -- default it is simply the debian version number.  The version
      -- number in the source tree should not have our vendor tag,
      -- that should only be added by the autobuilder.
      sourceRevision = {-T.revision-} T.method (download (tgt target))
      sourceVersion = logVersion sourceLog
      sourceLog = entry . cleanSource $ target
      sourceDependencies = fromList $ List.map makeVersion buildDependencySolution
-- |Convert to a simple name and version record to interface with older
-- code.

makeVersion :: BinaryPackage -> PackageID BinPkgName
makeVersion package =
    PackageID { packageName = packageName (packageID package)
              , packageVersion = packageVersion (packageID package) }

-- |Decide whether to build a package.  We will build if the revision
-- is different from the revision of the uploaded source, or if any of
-- the build dependencies are newer than the versions which were
-- encoded into the uploaded version's control file.
buildDecision :: P.CacheRec
              -> Target
              -> Maybe DownstreamFingerprint -- ^ The fingerprint of the most recent build
              -> Fingerprint -- ^ The fingerprint of the source package
              -> SourcePackageStatus -- ^ The status of the version in the repository with respect
                                     -- to the architecture we are building - either all binary packages
                                     -- are available, or none, or only the architecture independent.
              -> BuildDecision
buildDecision cache target _ _ _ | elem (debianSourcePackageName (debianSourceTree (tgt target))) (P.forceBuild (P.params cache)) = Yes "--force-build option is set"
buildDecision _ target _ (Fingerprint {upstreamVersion = sourceVersion}) _
    | any (== (show (prettyDebianVersion sourceVersion))) (mapMaybe skipVersion . P.flags . T.package . download . tgt $ target) =
        No ("Skipped version " ++ show (prettyDebianVersion sourceVersion))
    where
      skipVersion (P.SkipVersion s) = Just s
      skipVersion _ = Nothing
buildDecision _ target _ (Fingerprint {upstreamVersion = sourceVersion}) _
    | any (== (show (prettyDebianVersion sourceVersion))) (mapMaybe failVersion . P.flags . T.package . download . tgt $ target) =
        No ("Failed version " ++ show (prettyDebianVersion sourceVersion))
    where
      failVersion (P.FailVersion s) = Just s
      failVersion _ = Nothing
buildDecision _ target _ _ _
    | any isSkipPackage (P.flags . T.package . download . tgt $ target) =
        No "Skipped"
    where isSkipPackage P.SkipPackage = True
          isSkipPackage _ = False
buildDecision _ target _ _ _
    | any isFailPackage (P.flags . T.package . download . tgt $ target) =
        Error "FailPackage specified"
    where isFailPackage P.FailPackage = True
          isFailPackage _ = False
buildDecision _ _ Nothing (Fingerprint {upstreamVersion = sourceVersion}) _ =
    Yes ("Initial build of version " ++ show (prettyDebianVersion sourceVersion))
buildDecision _ _ (Just (DownstreamFingerprint {upstreamFingerprint = Fingerprint {method = oldMethod}})) (Fingerprint {method = newMethod}) _
    | oldMethod /= newMethod = Yes ("Retrieve method changed: " ++ show oldMethod ++ " -> " ++ show newMethod)
buildDecision cache target (Just (DownstreamFingerprint { upstreamFingerprint =
                                                              Fingerprint { upstreamVersion = oldSrcVersion
                                                                          , buildDependencyVersions = builtDependencies }
                                                        , downstreamVersion = repoVersion })) -- I suspect oldSrcVersion is always equal to repoVersion
                           (Fingerprint {upstreamVersion = sourceVersion, buildDependencyVersions = sourceDependencies})
                           releaseStatus =
    case compare sourceVersion oldSrcVersion of
      GT -> Yes ("Source version (" ++ show (prettyDebianVersion sourceVersion) ++ ") is newer than released source version (" ++ show (prettyDebianVersion oldSrcVersion) ++ ")")
      LT -> No ("Source version (" ++ show (prettyDebianVersion sourceVersion) ++ ") is trumped by released source version (" ++ show (prettyDebianVersion oldSrcVersion) ++ ")")
      EQ -> sameSourceTests
    where
      -- discardTarget = Set.member (targetName target) (P.discard (P.params cache))
      allowBuildDependencyRegressions = P.allowBuildDependencyRegressions (P.params cache)
      -- Build decision tests for when the version number of the
      -- source hasn't changed.  Note that the source itself may have
      -- changed, but we don't ask the SCCS whether that has happened.
      -- This is a design decision which avoids building from source
      -- that might have been checked in but isn't ready to be
      -- uploaded to the repository.  Note that if the build
      -- dependencies change the package will be built anyway, so we
      -- are not completely protected from this possibility.
      sameSourceTests =
          case releaseStatus of
            Indep missing | missing /= [] && not (notArchDep target) ->
                  -- The binary packages are missing, we need an arch only build.
                  Arch ("Version " ++ show (prettyDebianVersion repoVersion) ++ " needs arch only build. (Missing: " ++ show missing ++ ")")
            _ | badDependencies /= [] && not allowBuildDependencyRegressions ->
                  Error ("Build dependency regression (allow with --allow-build-dependency-regressions): " ++ 
                         concat (intersperse ", " (List.map (\ ver -> show (prettySimpleRelation (builtVersion ver)) ++ " -> " ++ show (prettySimpleRelation (Just ver))) badDependencies)))
              | badDependencies /= [] ->
                  Auto ("Build dependency regression: " ++ 
                        concat (intersperse ", " (List.map (\ ver -> show (prettySimpleRelation (builtVersion ver)) ++ " -> " ++ show (prettySimpleRelation (Just ver))) badDependencies)))
              | (revvedDependencies ++ newDependencies) /= [] ->
                  -- If the package *was* previously built by the autobuilder we rebuild when any
                  -- of its build dependencies are revved or new ones appear.
                  Auto ("Build dependencies changed:\n" ++ displayDependencyChanges (revvedDependencies ++ newDependencies))
            Indep _ | notArchDep target ->
                  No ("Version " ++ show (prettyDebianVersion sourceVersion) ++ " of architecture independent package is already in release.")
            Indep missing ->
                  -- The binary packages are missing, we need an arch only build.
                  Arch ("Version " ++ show (prettyDebianVersion repoVersion) ++ " needs arch only build. (Missing: " ++ show missing ++ ")")
            All ->
                  No ("Version " ++ show (prettyDebianVersion sourceVersion) ++ " is already in release.")
            _ ->
                  error ("Unexpected releaseStatus: " ++ show releaseStatus)
      notArchDep = all (== Just "all") . List.map (fieldValue "Architecture") . debianBinaryParagraphs
      displayDependencyChanges dependencies =
          "  " ++ intercalate "\n  " changes
          where
            changes = List.map (\ (built, new) -> show (prettySimpleRelation built) ++ " -> " ++ show (prettySimpleRelation (Just new))) (zip builtVersions dependencies)
            builtVersions = List.map findDepByName dependencies
            findDepByName new = listToMaybe $ Set.toList $ Set.filter (\ old -> packageName new == packageName old) builtDependencies
      -- If we are deciding whether to rebuild the same version of the source package,
      -- this function checks the status of the build dependencies.  If any are older
      -- now than when the package was built previously, it is a fatal error.  Probably
      -- the sources.list changed so that build dependency versions are no longer
      -- available, or some of the build dependencies were never built for the current
      -- build architecture.  If any are younger, we need to rebuild the package.
      -- buildDependencyStatus :: ([PkgVersion], [PkgVersion], [PkgVersion], [PkgVersion])
      (badDependencies, revvedDependencies, newDependencies, _unchangedDependencies) =
          (bad, changed, new, unchanged)
          where
            -- If any dependency is older than the one we last built with it is an error.
            (bad, notBad) = partition isOlder sourceDependencies'
            isOlder x = maybe False (\ built -> packageVersion built > packageVersion x) (builtVersion x)
            -- If a dependency is newer it generally triggers a rebuild.
            (changed, notChanged) = partition isNewer notBad
            isNewer x = maybe False (\ built -> packageVersion built < packageVersion x) (builtVersion x)
	    -- Dependencies which we have never seen before also generally trigger a rebuild.
            (new, unchanged) = partition (isNothing . builtVersion) notChanged
	    -- What version of this dependency was most recently used to build?
      builtVersion x = maybe Nothing (\ ver -> Just (PackageID (packageName x) ver)) (Map.findWithDefault Nothing (packageName x) builtDeps)
      builtDeps = Map.fromList (Set.toList (Set.map (\ p -> (packageName p, Just (packageVersion p))) builtDependencies))
      -- Remove any package not mentioned in the relaxed dependency list
      -- from the list of build dependencies which can trigger a rebuild.
      sourceDependencies' = toList $ Set.filter (\ x -> elem (packageName x) (packageNames (targetRelaxed (relaxDepends cache (tgt target)) target))) sourceDependencies
      -- All the package names mentioned in a dependency list
      packageNames :: G.DepInfo -> [BinPkgName]
      packageNames info {-(_, deps, _)-} = nub (List.map (\ (Rel name _ _) -> name) (concat (G.relations info)))
