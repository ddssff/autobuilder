{-# LANGUAGE CPP, OverloadedStrings #-}
module Debian.AutoBuilder.Types.Fingerprint
    ( targetFingerprint
    , buildDecision
    ) where

#if 0
import Control.Applicative ((<$>))
import Control.Monad.State (State, evalState, get, put)
import Data.Char (isSpace)
#endif
import Data.List as List (intercalate, intersperse, map, nub, partition)
import qualified Data.Map as Map
import Data.Maybe(isNothing, listToMaybe, mapMaybe)
import Data.Set as Set (toList, fromList, map, filter)
import Debian.AutoBuilder.Types.Buildable (Target(tgt, cleanSource), Buildable(download), targetRelaxed, relaxDepends)
import qualified Debian.AutoBuilder.Types.CacheRec as P
import qualified Debian.AutoBuilder.Types.Download as T
import qualified Debian.AutoBuilder.Types.ParamRec as P
import qualified Debian.AutoBuilder.Types.Packages as P
import Debian.Changes (logVersion)
import Debian.Control (fieldValue, debianBinaryParagraphs)
import qualified Debian.GenBuildDeps as G
import Debian.Relation (Relation(Rel), BinPkgName(..))
import Debian.Repo.Dependencies (prettySimpleRelation)
import Debian.Repo.Fingerprint as P
import Debian.Repo.SourceTree (HasChangeLog(entry), SourcePackageStatus(..), BuildDecision(..))
import Debian.Repo.PackageID (PackageID(PackageID, packageName, packageVersion))
import Debian.Repo.PackageIndex (BinaryPackage(packageID))
import Debian.Version (prettyDebianVersion)

targetFingerprint :: T.Download a => Target a -> [BinaryPackage] -> Fingerprint
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
buildDecision :: T.Download a =>
                 P.CacheRec
              -> Target a
              -> Maybe DownstreamFingerprint -- ^ The fingerprint of the most recent build
              -> Fingerprint -- ^ The fingerprint of the source package
              -> SourcePackageStatus -- ^ The status of the version in the repository with respect
                                     -- to the architecture we are building - either all binary packages
                                     -- are available, or none, or only the architecture independent.
              -> BuildDecision
buildDecision _cache _target Nothing upstream _releaseStatus =
    -- I am not sure whether the next older usable version is showing up here, or only
    -- packages with the same upstream version number.  We need the next older version
    -- to show up here to implement ignoreNewVersions properly.
    Yes ("Initial build of package " ++ show (prettyDebianVersion (upstreamVersion upstream)))
buildDecision cache target (Just downstream) upstream releaseStatus =
    case () of
      _ | skipVersion newSrcVersion ->
            No ("Skipped version " ++ show (prettyDebianVersion newSrcVersion))
        | failVersion newSrcVersion ->
            Error ("Failed version " ++ show (prettyDebianVersion newSrcVersion))
        | skipPackage ->
            No "Skipped"
        | failPackage ->
            Error "FailPackage specified"
        | oldAttrs /= newAttrs && not (specialAptVersionCase (toList oldAttrs) (toList newAttrs)) ->
            Yes ("Package attributes changed: " ++ show oldAttrs ++ " -> " ++ show newAttrs)
        | not (P.ignoreNewVersions (P.params cache)) && compare newSrcVersion oldSrcVersion == GT ->
            Yes ("Source version (" ++ show (prettyDebianVersion newSrcVersion) ++ ") is newer than released source version (" ++ show (prettyDebianVersion oldSrcVersion) ++ ")")
        | compare newSrcVersion oldSrcVersion == LT ->
            No ("Source version (" ++ show (prettyDebianVersion newSrcVersion) ++ ") is trumped by released source version (" ++ show (prettyDebianVersion oldSrcVersion) ++ ")")
        | not (notArchDep target) && not (null missingDebs) ->
            Arch ("Version " ++ show (prettyDebianVersion repoVersion) ++ " needs arch only build. (Missing: " ++ show missingDebs ++ ")")
        | badDependencies /= [] && not allowBuildDependencyRegressions ->
            Error ("Build dependency regression (allow with --allow-build-dependency-regressions): " ++
                   concat (intersperse ", " (List.map (\ ver -> show (prettySimpleRelation (builtVersion ver)) ++ " -> " ++ show (prettySimpleRelation (Just ver))) badDependencies)))
        | badDependencies /= [] ->
            Auto ("Build dependency regression: " ++
                  concat (intersperse ", " (List.map (\ ver -> show (prettySimpleRelation (builtVersion ver)) ++ " -> " ++ show (prettySimpleRelation (Just ver))) badDependencies)))
        | (not $ null $ revvedDependencies ++ newDependencies) ->
            -- If the package *was* previously built by the autobuilder we rebuild when any
            -- of its build dependencies are revved or new ones appear.
            Auto ("Build dependencies changed:\n" ++ displayDependencyChanges (revvedDependencies ++ newDependencies) ++
                  "\n  Note that if a new version appears in hackage, and it has dependencies that the current build\n" ++
                  "does not, an unnecessary build will be triggered here even if --ignore-new-versions is used.  The\n" ++
                  "workaround for this is to use CabalPin.")
        | isArchIndep && notArchDep target ->
            No ("Version " ++ show (prettyDebianVersion oldSrcVersion) ++ " of architecture independent package is already in release.")
        | isArchIndep ->
            -- The binary packages are missing, we need an arch only build.
            Arch ("Version " ++ show (prettyDebianVersion repoVersion) ++ " needs arch only build. (Missing: " ++ show missingDebs ++ ")")
        | releaseStatus == All ->
            No ("Version " ++ show (prettyDebianVersion oldSrcVersion) ++ " is already in release.")
      _ -> -- releaseStatus == None ->
            error ("Unexpected releaseStatus: " ++ show releaseStatus)
    where
      allowBuildDependencyRegressions = P.allowBuildDependencyRegressions (P.params cache)

      newAttrs = retrievedAttributes upstream
      newSrcVersion = upstreamVersion upstream
      sourceDependencies = buildDependencyVersions upstream
      oldAttrs = retrievedAttributes (upstreamFingerprint downstream)
      oldSrcVersion = upstreamVersion (upstreamFingerprint downstream)
      builtDependencies = buildDependencyVersions (upstreamFingerprint downstream)
      repoVersion = downstreamVersion downstream -- I suspect oldSrcVersion is always equal to repoVersion

      skipVersion v = any (== (show v)) (mapMaybe (\x -> case x of P.SkipVersion s -> Just s; _ -> Nothing) . T.flags . download . tgt $ target)
      failVersion v = any (== (show v)) (mapMaybe (\x -> case x of P.FailVersion s -> Just s; _ -> Nothing) . T.flags . download . tgt $ target)
      skipPackage = any (\x -> case x of P.SkipPackage -> True; _ -> False) (T.flags . download . tgt $ target)
      failPackage = any (\x -> case x of P.FailPackage -> True; _ -> False) (T.flags . download . tgt $ target)
      isArchIndep = case releaseStatus of
                      Indep _ -> True
                      _ -> False
      notArchDep = all (== Just "all") . List.map (fieldValue "Architecture") . debianBinaryParagraphs
      missingDebs = case releaseStatus of
                      Indep xs -> xs
                      _ -> []
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

-- Special case - if all that changed is that an AptVersion attribute
-- was added, do not build.  Debian version number changes currently
-- trigger builds anyway through other mechanisms.  Right now I just
-- don't want to rebuild ghc.
specialAptVersionCase [] [AptVersion _] = True
specialAptVersionCase _ _ = False
