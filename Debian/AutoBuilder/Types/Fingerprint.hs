{-# LANGUAGE OverloadedStrings #-}
module Debian.AutoBuilder.Types.Fingerprint
    ( Fingerprint
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
import Data.Generics (everywhere, mkT)
import Data.List (intercalate, intersperse, find, partition, nub)
import qualified Data.Map as Map
import Data.Maybe(fromJust, isJust, isNothing, mapMaybe)
import qualified Data.Set as Set
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
import Debian.VersionPolicy(dropTag, parseTag)
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
        , upstreamVersion :: Maybe DebianVersion
          -- ^ The version number in the changelog of the freshly downloaded
          -- package, before any suffix is added by the autobuilder.
        , buildDependencyVersions :: [PackageID BinPkgName]
          -- ^ The names and version numbers of the build dependencies which
          -- were present when the package was build.
        , downstreamVersion :: Maybe DebianVersion
          -- ^ This will be the same as the version field plus
          -- the suffix that was added by the autobuilder.
        }
    | NoFingerprint
    deriving Show

readMethod :: String -> Maybe P.RetrieveMethod
readMethod s = maybeRead s

packageFingerprint :: Maybe SourcePackage -> Fingerprint
packageFingerprint Nothing = NoFingerprint
packageFingerprint (Just package) =
    maybe NoFingerprint (parseRevision . unpack) (S.fieldValue "Fingerprint" . sourceParagraph $ package)
    where
      parseRevision s =
          case reads s :: [(String, String)] of
            [(method, etc)] ->
                case readMethod method of
                  Nothing -> NoFingerprint
                  Just method' ->
                      let method'' = modernizeMethod method' in
                      case words etc of
                        (sourceVersion : buildDeps)
                          | not (elem '=' sourceVersion) ->
                              Fingerprint
                                { method = method''
                                , upstreamVersion = Just (parseDebianVersion sourceVersion)
                                , buildDependencyVersions = map readSimpleRelation buildDeps
                                , downstreamVersion = Just . packageVersion . sourcePackageID $ package }
                        buildDeps ->
                            Fingerprint
                              { method = method''
                              , upstreamVersion = Nothing
                              , buildDependencyVersions = map readSimpleRelation buildDeps
                              , downstreamVersion = Just . packageVersion . sourcePackageID $ package }
            _ -> NoFingerprint

modernizeMethod :: P.RetrieveMethod -> P.RetrieveMethod
modernizeMethod = everywhere (mkT modernizeMethod1)

modernizeMethod1 :: P.RetrieveMethod -> P.RetrieveMethod
modernizeMethod1 (P.Debianize p) = P.Debianize' p []
modernizeMethod1 x = x

showFingerprint :: Fingerprint -> S.Field
showFingerprint (Fingerprint {method = method, upstreamVersion = Just sourceVersion, buildDependencyVersions = versions}) =
    S.Field ("Fingerprint", " " ++ show (show method) ++ " " ++ show (prettyDebianVersion sourceVersion) ++ " " ++ intercalate " " (map showSimpleRelation versions))
showFingerprint _ = error "missing fingerprint info"

showDependencies :: Fingerprint -> [String]
showDependencies (Fingerprint {buildDependencyVersions = deps}) = map showSimpleRelation deps
showDependencies _ = []

-- | Show the dependency list without the version numbers.
showDependencies' :: Fingerprint -> [String]
showDependencies' (Fingerprint {buildDependencyVersions = deps}) = map (show . pretty . packageName) deps
showDependencies' _ = []

dependencyChanges :: Fingerprint -> Fingerprint -> String
dependencyChanges old new =
    depChanges changedDeps
    where
      depChanges [] = ""
      depChanges _ = "  * Build dependency changes:" ++ prefix ++ intercalate prefix padded ++ "\n"
      padded = map concat . columns . map showDepChange $ changedDeps
      changedDeps = Set.toList (Set.difference (Set.fromList (deps new)) (Set.fromList (deps new)))
      showDepChange newDep =
          case filter (hasName (packageName newDep)) (deps old) of
            [] -> [" " ++ unBinPkgName (packageName newDep) ++ ": ", "(none)", " -> ", show (prettyDebianVersion (packageVersion newDep))]
            (oldDep : _) -> [" " ++ unBinPkgName (packageName newDep) ++ ": ", show (prettyDebianVersion (packageVersion oldDep)), " -> ", show (prettyDebianVersion (packageVersion newDep))]
      hasName name = ((== name) . packageName)
      prefix = "\n    "
      deps (Fingerprint {buildDependencyVersions = x}) = x
      deps NoFingerprint = []

targetFingerprint :: Target -> [BinaryPackage] -> Fingerprint
targetFingerprint target buildDependencySolution =
    Fingerprint { method = sourceRevision
                 , upstreamVersion = Just sourceVersion
                 , buildDependencyVersions = sourceDependencies
                 , downstreamVersion = Nothing }
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
      sourceDependencies = map makeVersion buildDependencySolution
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
              -> Fingerprint            -- ^ The fingerprint of the most recent build
              -> Fingerprint            -- ^ The fingerprint of the source package
              -> SourcePackageStatus	-- ^ The status of the version in the repository with respect
                                        -- to the architecture we are building - either all binary packages
                                        -- are available, or none, or only the architecture independent.
              -> BuildDecision
buildDecision cache target _ _ _ | elem (debianSourcePackageName (debianSourceTree (tgt target))) (P.forceBuild (P.params cache)) = Yes "--force-build option is set"
buildDecision _ target _ (Fingerprint {upstreamVersion = Just sourceVersion}) _
    | any (== (show (prettyDebianVersion sourceVersion))) (mapMaybe skipVersion . P.flags . T.package . download . tgt $ target) =
        No ("Skipped version " ++ show (prettyDebianVersion sourceVersion))
    where
      skipVersion (P.SkipVersion s) = Just s
      skipVersion _ = Nothing
buildDecision _ target _ (Fingerprint {upstreamVersion = Just sourceVersion}) _
    | any (== (show (prettyDebianVersion sourceVersion))) (mapMaybe failVersion . P.flags . T.package . download . tgt $ target) =
        No ("Failed version " ++ show (prettyDebianVersion sourceVersion))
    where
      failVersion (P.FailVersion s) = Just s
      failVersion _ = Nothing
buildDecision _ target _ (Fingerprint {upstreamVersion = Just _}) _
    | any isSkipPackage (P.flags . T.package . download . tgt $ target) =
        No "Skipped"
    where isSkipPackage P.SkipPackage = True
          isSkipPackage _ = False
buildDecision _ target _ (Fingerprint {upstreamVersion = Just _}) _
    | any isFailPackage (P.flags . T.package . download . tgt $ target) =
        Error "FailPackage specified"
    where isFailPackage P.FailPackage = True
          isFailPackage _ = False
buildDecision _ _ NoFingerprint (Fingerprint {upstreamVersion = Just sourceVersion}) _ =
    Yes ("Initial build of version " ++ show (prettyDebianVersion sourceVersion))
buildDecision _ _ (Fingerprint {method = oldMethod}) (Fingerprint {method = newMethod}) _
    | oldMethod /= newMethod = Yes ("Retrieve method changed: " ++ show oldMethod ++ " -> " ++ show newMethod)
buildDecision _ _ _ NoFingerprint _ = error "Missing source fingerprint"
buildDecision _ _ _ (Fingerprint {upstreamVersion = Nothing}) _ = error "Missing source version"
buildDecision cache target (Fingerprint {upstreamVersion =oldSrcVersion, buildDependencyVersions = builtDependencies, downstreamVersion = repoVersion}) -- I suspect oldSrcVersion is always equal to repoVersion
                           (Fingerprint {upstreamVersion = Just sourceVersion, buildDependencyVersions = sourceDependencies})
                           releaseStatus =
    case isJust oldSrcVersion of
      True ->
          case compare sourceVersion (fromJust oldSrcVersion) of
            GT -> Yes ("Source version (" ++ show (prettyDebianVersion sourceVersion) ++ ") is newer than released source version (" ++ show (prettyDebianVersion (fromJust oldSrcVersion)) ++ ")")
            LT -> No ("Source version (" ++ show (prettyDebianVersion sourceVersion) ++ ") is trumped by released source version (" ++ show (prettyDebianVersion (fromJust oldSrcVersion)) ++ ")")
            EQ -> sameSourceTests
      False ->
          case compare (dropTag allTags sourceVersion) (dropTag allTags (fromJust repoVersion)) of
            GT -> Yes ("Source version (" ++ show (prettyDebianVersion sourceVersion) ++ ") is newer than released source version (" ++ show (prettyDebianVersion (fromJust repoVersion)) ++ ")")
            LT -> No ("Source version (" ++ show (prettyDebianVersion sourceVersion) ++ ") is trumped by released source version (" ++ show (prettyDebianVersion (fromJust repoVersion)) ++ ")")
            EQ ->
                case dropTag allTags sourceVersion == sourceVersion of
                  False -> Yes ("Source version (" ++ show (prettyDebianVersion sourceVersion) ++ ") is tagged, and old source version was not recorded")
                  True -> sameSourceTests
    where
      vendorTag = P.vendorTag (P.params cache)
      oldVendorTags = P.oldVendorTags (P.params cache)
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
                  Arch ("Version " ++ maybe "Nothing" show (fmap prettyDebianVersion repoVersion) ++ " needs arch only build. (Missing: " ++ show missing ++ ")")
            _ | badDependencies /= [] && not allowBuildDependencyRegressions ->
                  Error ("Build dependency regression (allow with --allow-build-dependency-regressions): " ++ 
                         concat (intersperse ", " (map (\ ver -> show (prettySimpleRelation (builtVersion ver)) ++ " -> " ++ show (prettySimpleRelation (Just ver))) badDependencies)))
              | badDependencies /= [] ->
                  Auto ("Build dependency regression: " ++ 
                        concat (intersperse ", " (map (\ ver -> show (prettySimpleRelation (builtVersion ver)) ++ " -> " ++ show (prettySimpleRelation (Just ver))) badDependencies)))
              | autobuiltDependencies /= [] && isNothing oldSrcVersion ->
		  -- If oldSrcVersion is Nothing, the autobuilder didn't make the previous build
                  -- so there are no recorded build dependencies.  In that case we don't really
                  -- know whether a build is required, so we could go either way.  The decision
                  -- here is to only built if some of the build dependencies were built by the
                  -- autobuilder (so their version numbers have been tagged by it.)
                  Auto ("Build dependency status unknown:\n" ++ displayDependencyChanges autobuiltDependencies)
              | (revvedDependencies ++ newDependencies) /= [] && isJust oldSrcVersion ->
                  -- If the package *was* previously built by the autobuilder we rebuild when any
                  -- of its build dependencies are revved or new ones appear.
                  Auto ("Build dependencies changed:\n" ++ displayDependencyChanges (revvedDependencies ++ newDependencies))
            Indep _ | notArchDep target ->
                  No ("Version " ++ show (prettyDebianVersion sourceVersion) ++ " of architecture independent package is already in release.")
            Indep missing ->
                  -- The binary packages are missing, we need an arch only build.
                  Arch ("Version " ++ maybe "Nothing" show (fmap prettyDebianVersion repoVersion) ++ " needs arch only build. (Missing: " ++ show missing ++ ")")
            All ->
                  No ("Version " ++ show (prettyDebianVersion sourceVersion) ++ " is already in release.")
            _ ->
                  error ("Unexpected releaseStatus: " ++ show releaseStatus)
      notArchDep = all (== Just "all") . map (fieldValue "Architecture") . debianBinaryParagraphs
      displayDependencyChanges dependencies =
          "  " ++ intercalate "\n  " changes
          where
            changes = map (\ (built, new) -> show (prettySimpleRelation built) ++ " -> " ++ show (prettySimpleRelation (Just new))) (zip builtVersions dependencies)
            builtVersions = map findDepByName dependencies
            findDepByName new = find (\ old -> packageName new == packageName old) builtDependencies
      -- The list of the revved and new dependencies which were built by the autobuilder.
      autobuiltDependencies = filter isTagged (revvedDependencies ++ newDependencies)
      -- Versions we generated, with vendor and release tags
      isTagged :: PackageID BinPkgName -> Bool
      isTagged dep = isJust . snd . parseTag allTags . packageVersion $ dep
      allTags :: [String]
      allTags = vendorTag : oldVendorTags
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
      builtDeps = Map.fromList (map (\ p -> (packageName p, Just (packageVersion p))) builtDependencies)
      -- Remove any package not mentioned in the relaxed dependency list
      -- from the list of build dependencies which can trigger a rebuild.
      sourceDependencies' = filter (\ x -> elem (packageName x) (packageNames (targetRelaxed (relaxDepends cache (tgt target)) target))) sourceDependencies
      -- All the package names mentioned in a dependency list
      packageNames :: G.DepInfo -> [BinPkgName]
      packageNames info {-(_, deps, _)-} = nub (map (\ (Rel name _ _) -> name) (concat (G.relations info)))
