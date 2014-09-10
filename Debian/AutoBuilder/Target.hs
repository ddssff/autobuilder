{-# LANGUAGE BangPatterns, FlexibleContexts, OverloadedStrings, PackageImports, RankNTypes, ScopedTypeVariables, StandaloneDeriving, TemplateHaskell, TypeFamilies #-}
{-# OPTIONS -Wall -fwarn-unused-imports -fno-warn-name-shadowing -fno-warn-orphans #-}
-- |A Target represents a particular set of source code and the
-- methods to retrieve and update it.
-- 
-- Author: David Fox <ddssff@gmail.com>
module Debian.AutoBuilder.Target
    ( changelogText	-- Tgt -> Maybe String -> [PkgVersion] -> String
    , buildTargets
    , showTargets
    , decode
    , display
    ) where

import Control.Applicative ((<$>))
import Control.Applicative.Error (Failing(..))
import Control.Arrow (second)
import Control.Exception (AsyncException(UserInterrupt), evaluate, fromException, SomeException, throw, toException)
import Control.Monad.Catch (MonadCatch, catch, try, MonadMask)
import Control.Monad.RWS (liftIO, MonadIO, when)
import qualified Data.ByteString.Char8 as B (concat)
import qualified Data.ByteString.Lazy.Char8 as L (ByteString, empty, toChunks, unpack)
import qualified Data.ByteString.UTF8 as UTF8 (toString)
import Data.Either (partitionEithers)
import Data.Function (on)
import Data.List (intercalate, intersect, intersperse, isSuffixOf, nub, partition, sortBy)
import Data.Maybe (catMaybes, fromJust, isNothing, listToMaybe)
import Data.Monoid ((<>))
import qualified Data.Set as Set (difference, empty, fromList, insert, member, null, partition, Set, size, toList, union)
import qualified Data.Text as T (pack, unpack)
import Data.Time (NominalDiffTime)
import Debian.Arch (Arch)
import qualified Debian.AutoBuilder.Params as P (baseRelease, isDevelopmentRelease)
import Debian.AutoBuilder.Types.Buildable (Buildable(..), failing, prepareTarget, relaxDepends, Target(tgt, cleanSource, targetDepends), targetRelaxed)
import qualified Debian.AutoBuilder.Types.CacheRec as P (CacheRec(params))
import qualified Debian.AutoBuilder.Types.Download as T (Download(buildWrapper, getTop, logText), flags, method)
import Debian.AutoBuilder.Types.Fingerprint (buildDecision, targetFingerprint)
import qualified Debian.AutoBuilder.Types.Packages as P (foldPackages, packageCount, PackageFlag(UDeb), Packages)
import qualified Debian.AutoBuilder.Types.ParamRec as P (ParamRec(autobuilderEmail, buildDepends, buildRelease, buildTrumped, discard, doNotChangeVersion, dryRun, extraReleaseTag, noClean, oldVendorTags, preferred, releaseAliases, setEnv, strictness, vendorTag), Strictness(Lax))
import qualified Debian.AutoBuilder.Version as V (autoBuilderVersion)
import Debian.Changes (ChangedFileSpec(changedFileSize, changedFileName, changedFileMD5sum, changedFileSHA1sum, changedFileSHA256sum), ChangeLogEntry(logWho, logVersion, logDists, logDate, logComments), ChangesFile(changeRelease, changeInfo, changeFiles, changeDir))
import Debian.Control (Control'(Control), ControlFunctions(parseControlFromFile), Field'(Comment, Field), fieldValue, Paragraph'(..), raiseFields, HasDebianControl, debianSourcePackageName)
import qualified Debian.GenBuildDeps as G (buildable, BuildableInfo(CycleInfo, readyTargets), ReadyTarget(..), buildDependencies, compareSource, DepInfo(binaryNames, relations, sourceName))
import Debian.Pretty (Pretty(pretty), display)
import Debian.Relation (BinPkgName(..), SrcPkgName(..))
import Debian.Relation.ByteString (Relation(..), Relations)
import Debian.Release (ReleaseName(relName), releaseName')
import Debian.Repo.Fingerprint (dependencyChanges, DownstreamFingerprint, Fingerprint, packageFingerprint, showDependencies', showFingerprint)
import Debian.Repo.Internal.Apt (MonadApt)
import Debian.Repo.Internal.Repos (MonadRepos)
import Debian.Repo.MonadOS (MonadOS(getOS), evalMonadOS, updateLists, withProc, withTmp, syncLocalPool, buildEssential, syncOS)
import Debian.Repo.OSImage (osRoot)
import Debian.Repo.Changes (saveChangesFile)
import Debian.Repo.Dependencies (prettySimpleRelation, simplifyRelations, solutions)
import Debian.Repo.EnvPath (EnvRoot(EnvRoot, rootPath))
import Debian.Repo.LocalRepository (LocalRepository, uploadLocal)
import Debian.Repo.Package (binaryPackageSourceVersion, sourcePackageBinaryNames)
import Debian.Repo.PackageID (PackageID(packageVersion))
import Debian.Repo.PackageIndex (BinaryPackage(packageInfo), prettyBinaryPackage, SourcePackage(sourceParagraph, sourcePackageID), sortBinaryPackages, sortSourcePackages)
import Debian.Repo.Prelude (symbol, readProc)
import Debian.Repo.SourceTree (addLogEntry, buildDebs, copySourceTree, DebianBuildTree, findChanges, findOneDebianBuildTree, SourcePackageStatus(..), BuildDecision(..), HasChangeLog(entry), HasDebDir(debdir), HasTopDir(topdir))
import Debian.Repo.State.AptImage (aptSourcePackages)
import Debian.Repo.State.OSImage (osSourcePackages, osBinaryPackages, updateOS, buildArchOfOS)
import Debian.Repo.State.Package (scanIncoming, InstallResult(Ok), showErrors, MonadInstall, evalInstall)
import Debian.Repo.Top (MonadTop)
import Debian.Time (getCurrentLocalRFC822Time)
import Debian.Version (DebianVersion, parseDebianVersion, prettyDebianVersion)
import Debian.VersionPolicy (parseTag, setTag)
import Extra.Files (replaceFile)
import "Extra" Extra.List (dropPrefix)
import Extra.Misc (columns)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, removeDirectory)
import System.Exit (ExitCode(ExitSuccess, ExitFailure), exitWith)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import System.Posix.Files (fileSize, getFileStatus)
import System.Process (CreateProcess(cwd, env), proc, readProcessWithExitCode, shell, showCommandForUser)
import Debian.Repo.Prelude.Verbosity (ePutStrLn, noisier, qPutStrLn, quieter, ePutStr, readProcFailing)
import System.Process.ListLike (collectProcessTriple, collectProcessOutput', collectOutputAndError')
import System.Unix.Chroot (useEnv)
import Text.Printf (printf)
import Text.Regex (matchRegex, mkRegex)

instance Ord Target where
    compare = compare `on` debianSourcePackageName

instance Monad Failing where
  return = Success
  m >>= f =
      case m of
        (Failure errs) -> (Failure errs)
        (Success a) -> f a
  fail errMsg = Failure [errMsg]

decode :: L.ByteString -> String
decode = UTF8.toString . B.concat . L.toChunks

-- |Generate the details section of the package's new changelog entry
-- based on the target type and version info.  This includes the
-- revision info and build dependency versions in a human readable
-- form.  FIXME: this should also include revision control log
-- entries.
changelogText :: Buildable -> Maybe DownstreamFingerprint -> Fingerprint -> String
changelogText buildable old new = ("  * " ++ T.logText (download buildable) ++ "\n" ++ dependencyChanges old new ++ "\n")

-- |Generate the string of build dependency versions:
-- package1=version1 package2=version2 ...
_formatVersions :: [PackageID BinPkgName] -> String
_formatVersions buildDeps =
    prefix ++
    intercalate prefix (map (show . prettySimpleRelation . Just) buildDeps) ++
    "\n"
    where prefix = "\n    "

prepareTargets :: (MonadOS m, MonadRepos m, MonadMask m) => P.CacheRec -> Relations -> [Buildable] -> m [Target]
prepareTargets cache globalBuildDeps targetSpecs =
    do results <- mapM (prepare (length targetSpecs)) (zip [1..] targetSpecs)
       let (failures, targets) = partitionEithers results
       let msg = "Could not prepare " ++ show (length failures) ++ " targets:\n" ++
                 concatMap (\ (n, e) -> printf "%4d. " n ++ show e ++ "\n") (zip [(1::Int)..] failures)
       case null failures of
         True -> return targets
         False -> ePutStr msg >> error msg
    where
      prepare :: (MonadOS m, MonadRepos m, MonadMask m) => Int -> (Int, Buildable) -> m (Either SomeException Target)
      prepare count (index, tgt) =
          do qPutStrLn (printf "[%2d of %2d] %s in %s" index count (display . debianSourcePackageName $ tgt) (T.getTop $ download $ tgt))
             try (prepareTarget cache globalBuildDeps tgt) >>=
                 either (\ (e :: SomeException) ->
                             ePutStrLn (printf "[%2d of %2d] - could not prepare %s: %s"
                                               index count (show (T.method (download tgt))) (show e)) >>
                             return (Left e))
                        (return . Right)

{-
partitionFailing :: [Failing a] -> ([[String]], [a])
partitionFailing xs =
    loop xs ([], [])
    where
      loop [] (fs, xs) = (fs, xs)
      loop (Success x : more) (fs, xs) = loop more (fs, x : xs)
      loop (Failure f : more) (fs, xs) = loop more (f : fs, xs)
-}

-- | Build a set of targets.  When a target build is successful it
-- is uploaded to the incoming directory of the local repository,
-- and then the function to process the incoming queue is called.
buildTargets :: (MonadRepos m, MonadTop m, MonadApt m, MonadMask m) =>
                P.CacheRec -> EnvRoot -> EnvRoot -> LocalRepository -> [Buildable] -> m (LocalRepository, [Target])
buildTargets _ _ _ localRepo [] = return (localRepo, [])
buildTargets cache dependOS buildOS localRepo !targetSpecs =
    do globalBuildDeps <- evalMonadOS buildEssential dependOS
       qPutStrLn ("\nAssembling source trees: (" ++ $(symbol 'buildTargets) ++ ")\n")
       targets <- evalMonadOS (prepareTargets cache globalBuildDeps targetSpecs) dependOS
       qPutStrLn "\nBuilding all targets:"
       failed <- buildLoop cache localRepo dependOS buildOS targets
       return (localRepo, failed)
 
-- Execute the target build loop until all the goals (or everything) is built
-- FIXME: Use sets instead of lists
buildLoop :: (MonadRepos m, MonadTop m, MonadApt m, MonadMask m) =>
             P.CacheRec -> LocalRepository -> EnvRoot -> EnvRoot -> [Target] -> m [Target]
buildLoop cache localRepo dependOS buildOS !targets =
    Set.toList <$> loop (Set.fromList targets) Set.empty
    where
      -- This loop computes the list of known ready targets and call
      -- loop2 to build them
      loop :: (MonadRepos m, MonadApt m, MonadTop m, MonadMask m) =>
              Set.Set Target -> Set.Set Target -> m (Set.Set Target)
      loop unbuilt failed | Set.null unbuilt = return failed
      loop unbuilt failed =
          ePutStrLn "\nComputing ready targets..." >>
          case readyTargets cache (Set.toList unbuilt) of
            [] -> return failed
            ready ->
                do ePutStrLn (makeTable ready)
                   loop2 (Set.difference unbuilt (Set.fromList $ map G.ready ready)) failed ready
      loop2 :: (MonadRepos m, MonadApt m, MonadTop m, MonadMask m) =>
               Set.Set Target -- unbuilt: targets which have not been built and are not ready to build
            -> Set.Set Target -- failed: Targets which either failed to build or were blocked by a target that failed to build
            -> [G.ReadyTarget Target] -- ready: the list of known buildable targets
            -> m (Set.Set Target)
      loop2 unbuilt failed [] =
          -- Out of ready targets, re-do the dependency computation
          loop unbuilt failed
      loop2 unbuilt failed (G.ReadyTarget {G.ready = target, G.waiting = blocked} : ready') =
          do ePutStrLn (printf "[%2d of %2d] TARGET: %s - %s"
                        (length targets - (Set.size unbuilt + length ready')) (length targets) (display . debianSourcePackageName $ target) (show (T.method (download (tgt target)))))
             -- Build one target.
             result <- if Set.member (debianSourcePackageName target) (P.discard (P.params cache))
                       then return (Failure ["--discard option set"])
                       else (Success <$> buildTarget cache dependOS buildOS localRepo target) `catch` handleBuildException
             failing -- On failure the target and its dependencies get
                     -- added to failed.
                     (\ errs ->
                          do ePutStrLn ("Package build failed:\n " ++ intercalate "\n " errs ++ "\n" ++
                                        "Discarding " ++ display (debianSourcePackageName target) ++ " and its dependencies:\n  " ++
                                        concat (intersperse "\n  " (map (display . debianSourcePackageName) blocked)))
                             let -- Remove the dependencies of the failed packages from unbuilt
                                 unbuilt' = Set.difference unbuilt (Set.fromList blocked)
                                 -- Add the target and its dependencies to failed
                                 failed' = Set.insert target  . Set.union (Set.fromList blocked) $ failed
                             loop2 unbuilt' failed' ready')
                     -- On success the target is discarded and its
                     -- dependencies are added to unbuilt.
                     (\ mRepo ->
                          do -- A new package has been uploaded, update the OS so it can be installed
                             maybe (return ()) (\ _ -> evalMonadOS updateOS dependOS) mRepo
                             -- Add to unbuilt any blocked packages that weren't already failed by
                             -- some other build
                             let unbuilt' = Set.union unbuilt (Set.difference (Set.fromList blocked) failed)
                             loop2 unbuilt' failed ready')
                     result
      handleBuildException :: MonadIO m => SomeException -> m (Failing (Maybe LocalRepository))
      handleBuildException e =
          liftIO (hPutStrLn stderr $ "Exception during build: " ++ show e) >>
          case (fromException (toException e) :: Maybe AsyncException) of
            Just UserInterrupt ->
                liftIO (hPutStrLn stderr "breaking out of loop") >>
                throw e -- break out of loop
            _ -> return (Failure [show e])
      -- If no goals are given in the build parameters, assume all
      -- known targets are goals.
{-
      goals targets =
          case P.goals (P.params cache) of
            [] -> targets
            goalNames -> filter (\ target -> elem (debianSourcePackageName target) goalNames) targets
-}

      -- Find the sources.list for the distribution we will be building in.
      --indent s = setStyle (addPrefix stderr s)
      --debugStyle = setStyle (cond Debian.IO.dryRun Debian.IO.realRun (P.debug params))

makeTable :: HasDebianControl control => [G.ReadyTarget control] -> String
makeTable ready =
    unlines . map (intercalate " ") . columns $ goalsLine ++ [[""]] ++ readyLines
    where
      goalsLine = []
      readyLines = map readyLine ready
      readyLine (G.ReadyTarget {G.ready = ready, G.waiting = blocked}) =
          [" Ready:", display (debianSourcePackageName ready), "Blocking " ++ show (length blocked) ++ ": [" ++ intercalate ", " (map (display . debianSourcePackageName) blocked) ++ "]"]

-- |Compute the list of targets that are ready to build from the build
-- dependency relations.  The return value is a list of target lists,
-- where the first element of each list is ready to build, and the
-- other elements are blocked by the first target.
--
-- It is not possible to precompute a build order for all the targets,
-- because we don't know ahead of time exactly what packages will be
-- built or even what their versions will be.  Furthermore, it is not
-- even desirable to do it that way, because then when we are actually
-- building the packages and one fails we no longer have enough
-- information to decide which other packages might still be
-- buildable.
--
-- Therefore, the algorithm we want to use is one where we look at
-- set of targets, choose one that can be built, build it, remove it
-- from the target set, and repeat until all targets are built.  We
-- can build a graph of the "has build dependency" relation and find
-- any node that has no inbound arcs and (maybe) build that.
readyTargets :: P.CacheRec -> {- [SrcPkgName] -> -} [Target] -> [G.ReadyTarget Target]
readyTargets cache targets =
    -- q12 "Choosing next target" $
    -- Compute the list of build dependency groups, each of which
    -- starts with a target that is ready to build followed by
    -- targets which are blocked by the first target.
    case G.buildable depends targets of
      (G.CycleInfo arcs) -> error (cycleMessage cache arcs)
      info ->
          case sortBy compareReady . G.readyTargets $ info of
            [] -> []
            ready -> ready
    where
      -- We choose the next target using the relaxed dependency set
      depends :: Target -> Target -> Ordering
      depends target1 target2 = G.compareSource (targetRelaxed (relaxDepends cache (tgt target1)) target1) (targetRelaxed (relaxDepends cache (tgt target2)) target2)
      -- Choose the next target to build.  Look for targets which are
      -- in the goal list, or which block packages in the goal list.
      -- Among those, prefer the target which blocks the most
      -- packages.  If there are goal targets but none of them are
      -- ready to build or directly block
      -- targets include a goal as readyamongoals none of the
      compareReady :: G.ReadyTarget a ->  G.ReadyTarget a -> Ordering
      -- Prefer targets which block more package
      compareReady = flip (compare `on` (length . G.waiting))

cycleMessage :: P.CacheRec -> [(Target, Target)] -> String
cycleMessage cache arcs =
    "Dependency cycles formed by these edges need to be broken:\n  " ++
    unlines (map (intercalate " ")
             (columns (["these binary packages", "from this source package", "", "force a rebuild of"] :
                       (map arcTuple arcs)))) ++
    "\nAdd one or more of these lines (but as few as possible) to your configuration file:\n  " ++
    intercalate "\n  " (map relaxLine (nub (concat (map pairs arcs))))
    where
      arcTuple (pkg, dep) =
          let rels = targetRelaxed (relaxDepends cache (tgt pkg)) pkg in
          [(show (intersect (binaryNames pkg dep) (binaryNamesOfRelations rels))), display (debianSourcePackageName dep), " -> ", display (debianSourcePackageName pkg)]
      relaxLine :: (BinPkgName, SrcPkgName) -> String
      relaxLine (bin, src) = "Relax-Depends: " ++ unBinPkgName bin ++ " " ++ unSrcPkgName src
      pairs :: (Target, Target) -> [(BinPkgName, SrcPkgName)]
      pairs (pkg, dep) =
          map (\ bin -> (bin, G.sourceName (targetDepends pkg))) binaryDependencies
              where binaryDependencies = intersect (binaryNames pkg dep) (binaryNamesOfRelations (targetRelaxed (relaxDepends cache (tgt pkg)) pkg))
      binaryNamesOfRelations :: G.DepInfo -> [BinPkgName]
      binaryNamesOfRelations info =
          concat (map (map (\ (Rel name _ _) -> name)) (G.relations info))
      binaryNames :: Target -> Target -> [BinPkgName]
      binaryNames pkg dep = G.binaryNames (targetRelaxed (relaxDepends cache (tgt pkg)) dep)

showTargets :: P.Packages -> String
showTargets targets =
    unlines (heading :
             map (const '-') heading :
             map concat (columns (reverse (snd (P.foldPackages (\ spec _flags (count, rows) -> (count + 1, [printf "%4d. " count, " ", limit 100 (show spec)] : rows)) targets (1 :: Int, []))))))
    where
      heading = show (P.packageCount targets) ++ " Targets:"

limit :: Int -> String -> String
limit n s = if length s > n + 3 then take n s ++ "..." else s

qError :: MonadIO m => String -> m b
qError message = qPutStrLn message >> error message

-- Decide whether a target needs to be built and, if so, build it.
buildTarget ::
    (MonadRepos m, MonadTop m, MonadApt m, MonadMask m) =>
    P.CacheRec ->			-- configuration info
    EnvRoot ->
    EnvRoot ->
    LocalRepository ->			-- ^ The local repository the packages will be uploaded to, this also may already contain packages.
    Target ->
    m (Maybe LocalRepository)	-- The local repository after the upload (if it changed)
buildTarget cache dependOS buildOS repo !target = do
  -- Get the control file from the clean source and compute the
  -- build dependencies
  arch <- evalMonadOS buildArchOfOS dependOS
  quieter 2 $ qPutStrLn "Looking for build dependency solutions..."
  soln <- evalMonadOS (buildDepSolution arch (map BinPkgName (P.preferred (P.params cache))) target) dependOS
  case soln of
        Failure excuses -> qError $ intercalate "\n  " ("Couldn't satisfy build dependencies" : excuses)
        Success packages ->
            do quieter 3 $ qPutStrLn ("Build dependency solution: " ++ show (map prettyBinaryPackage packages))
               -- Get the newest available version of a source package,
               -- along with its status, either Indep or All
               (releaseControlInfo, releaseStatus, _message) <- evalMonadOS (getReleaseControlInfo target) dependOS
               let repoVersion = fmap (packageVersion . sourcePackageID) releaseControlInfo
                   oldFingerprint = maybe Nothing packageFingerprint releaseControlInfo
               -- Get the changelog entry from the clean source
               let newFingerprint = targetFingerprint target packages
               -- qPutStrLn "Computing new version number of target package..."
               newVersion <- evalMonadOS (computeNewVersion cache target releaseControlInfo releaseStatus) dependOS
               let decision = buildDecision cache target oldFingerprint newFingerprint releaseStatus
               ePutStrLn ("Build decision: " ++ show decision)
               -- qPutStrLn ("newVersion: " ++ show (fmap prettyDebianVersion newVersion))
               -- qPutStrLn ("Release status: " ++ show releaseStatus)
               case newVersion of
                 Failure messages -> qError (intercalate "\n  " ("Failure computing new version number:" : messages))
                 Success version ->
                     -- If we are doing an arch only build, the version number needs to match the
                     -- version number of the architecture independent package already uploaded.
                     let buildVersion = case decision of
                                          Arch _ -> repoVersion
                                          _ -> Just version in
                     case decision of
                       Error message -> qError ("Failure making build decision: " ++ message)
                       No _ -> return Nothing
                       _ ->  buildPackage cache dependOS buildOS buildVersion oldFingerprint newFingerprint target decision repo >>=
                             return . Just

-- | Build a package and upload it to the local repository.
buildPackage :: (MonadRepos m, MonadTop m, MonadMask m) =>
                P.CacheRec -> EnvRoot -> EnvRoot -> Maybe DebianVersion -> Maybe DownstreamFingerprint -> Fingerprint -> Target -> BuildDecision -> LocalRepository -> m LocalRepository
buildPackage cache dependOS buildOS newVersion oldFingerprint newFingerprint !target decision repo = do
  checkDryRun
  source <- noisier 2 $ prepareBuildTree cache dependOS buildOS newFingerprint target
  logEntry source
  result <- evalMonadOS (build source) buildOS
  result' <- find result
  -- Upload to the local repo without a PGP key
  evalInstall (doLocalUpload result') repo Nothing
    where
      sourceLog = entry . cleanSource $ target
      checkDryRun = when (P.dryRun (P.params cache))
                      (do qPutStrLn "Not proceeding due to -n option."
                          liftIO (exitWith ExitSuccess))
      logEntry buildTree =
          case P.noClean (P.params cache) of
            False -> liftIO (maybeAddLogEntry buildTree newVersion)
            True -> return ()
      build :: forall m. (MonadOS m, MonadRepos m, MonadMask m) =>
               DebianBuildTree -> m (DebianBuildTree, NominalDiffTime)
      build buildTree =
          do -- The --commit flag does not appear until dpkg-dev-1.16.1,
             -- so we need to check this version number.  We also
             -- don't want to leave the patches subdirectory here
             -- unless we actually created a patch.
             root <- rootPath . osRoot <$> getOS
             let path = debdir buildTree
                 path' = fromJust (dropPrefix root path)
                 dpkgSource = (proc "dpkg-source" ["--commit", ".", "autobuilder.diff"]) {cwd = Just path', env = Just [("EDITOR", "/bin/true")]}
                 doDpkgSource False = do
                   createDirectoryIfMissing True (path' </> "debian/patches")
                   readProcFailing dpkgSource L.empty
                   exists <- doesFileExist (path' </> "debian/patches/autobuilder.diff")
                   when (not exists) (removeDirectory (path' </> "debian/patches"))
                 doDpkgSource True = readProcFailing dpkgSource L.empty >> return ()
                 -- doDpkgSource' = setEnv "EDITOR" "/bin/true" >> readCreateProcess ((proc "dpkg-source" ["--commit", ".", "autobuilder.diff"]) {cwd = Just path'}) L.empty
             _ <- liftIO $ useEnv' root (\ _ -> return ())
                             (-- Get the version number of dpkg-dev in the build environment
                              let p = shell ("dpkg -s dpkg-dev | sed -n 's/^Version: //p'") in
                              readProc p "" >>= collectProcessOutput' p >>= return . head . words . L.unpack >>= \ installed ->
                              -- If it is >= 1.16.1 we may need to run dpkg-source --commit.
                              readProc (shell ("dpkg --compare-versions '" ++ installed ++ "' ge 1.16.1")) "" >>= return . collectProcessTriple >>= \ (result, _, _) -> return (result == ExitSuccess) >>= \ newer ->
                              when newer (doesDirectoryExist (path' </> "debian/patches") >>= doDpkgSource)
                              {- when newer (do createDirectoryIfMissing True (path' </> "debian/patches")
                                             -- Create the patch if there are any changes
                                             _ <- lazyProcessF "dpkg-source" ["--commit", ".", "autobuilder.diff"] (Just path') Nothing L.empty
                                             -- If the patch was not created, remove the directory
                                             exists <- doesFileExist (path' </> "debian/patches/autobuilder.diff")
                                             when (not exists) (removeDirectory (path' </> "debian/patches"))) -}
                             )
             -- If newVersion is set, pass a parameter to cabal-debian
             -- to set the exact version number.
             let ver = maybe [] (\ v -> [("CABALDEBIAN", Just (show ["--deb-version", show (prettyDebianVersion v)]))]) newVersion
             let env = ver ++ P.setEnv (P.params cache)
             let action = buildDebs (P.noClean (P.params cache)) False env buildTree decision
             elapsed <- T.buildWrapper (download (tgt target)) action
             return (buildTree, elapsed)

      find (buildTree, elapsed) = liftIO (findChanges buildTree) >>= \ changesFile -> return (changesFile, elapsed)
      -- Depending on the strictness, build dependencies either
      -- get installed into the clean or the build environment.
      maybeAddLogEntry _ Nothing = return ()
      maybeAddLogEntry buildTree (Just newVersion) = getCurrentLocalRFC822Time >>= return . makeLogEntry newVersion >>= (flip addLogEntry) buildTree
      makeLogEntry newVersion date =
          sourceLog { logVersion = newVersion,
                      logDists = [P.buildRelease (P.params cache)],
                      logWho = P.autobuilderEmail (P.params cache),
                      logDate = date,
                      logComments = init (logComments sourceLog) ++ "\n" ++ changelogText (tgt target) oldFingerprint newFingerprint }
      setDistribution name changes =
          let (Paragraph fields) = changeInfo changes in
          let info' = map (setDist name) fields in
          changes { changeInfo = Paragraph info'
                  , changeRelease = name }
          where setDist name (Field ("Distribution", _)) = Field ("Distribution", " " <> T.pack (releaseName' name))
                setDist _ other = other
      doLocalUpload :: (MonadInstall m, MonadRepos m, MonadTop m, MonadMask m) => (ChangesFile, NominalDiffTime) -> m LocalRepository
      doLocalUpload (changesFile, elapsed) =
          do
            (changesFile' :: ChangesFile) <-
		-- Set the Distribution field in the .changes file to the one
                -- specified by the autobuilder Build-Release parameter.
                return (setDistribution (P.buildRelease (P.params cache)) changesFile) >>=
                -- Insert information about the build into the .changes file.
                liftIO . updateChangesFile elapsed >>=
                -- Insert the revision info into the .dsc file and update
                -- the md5sum of the .dsc file in the .changes file.
                liftIO . setRevisionInfo newFingerprint
            -- Upload to the local apt repository
            liftIO $ uploadLocal repo changesFile'
            -- The upload to the local repository is done even when
            -- the --dry-run flag is given.  Or it would be if we
            -- didn't exit when the first buildworthy target is found.
            results <- scanIncoming True Nothing repo
            case filter (not . (== Ok) . snd) results of
              -- Update lists to reflect the availability of the package we just built
              [] -> evalMonadOS (updateLists >> syncLocalPool) dependOS >> return repo
              _ -> error $ "Local upload failed:\n " ++ showErrors (map snd results)

-- |Prepare the build image by copying the clean image, installing
-- dependencies, and copying the clean source tree.  For a lax build
-- these operations take place in a different order from other types
-- of builds.  For lax: dependencies, then image copy, then source
-- copy.  For other: image copy, then source copy, then dependencies.
prepareBuildTree :: (MonadTop m, MonadRepos m, MonadMask m) => P.CacheRec -> EnvRoot -> EnvRoot -> Fingerprint -> Target -> m DebianBuildTree
prepareBuildTree cache dependOS buildOS sourceFingerprint target = do
  let dependRoot = rootPath dependOS
      buildRoot = rootPath buildOS
  let oldPath = topdir . cleanSource $ target
      newPath = buildRoot ++ fromJust (dropPrefix dependRoot oldPath)
  when (P.strictness (P.params cache) == P.Lax)
       (do -- Lax mode - dependencies accumulate in the dependency
           -- environment, sync that to build environment.
           _ <- evalMonadOS (installDependencies (cleanSource target) buildDepends sourceFingerprint) dependOS
           when (not noClean) (evalMonadOS (syncOS (EnvRoot buildRoot)) dependOS)
           return ())
  buildTree <- case noClean of
                 True ->
                     liftIO (findOneDebianBuildTree newPath) >>=
                     maybe (error ("No build tree at " ++ show newPath)) return
                 False ->
                     liftIO $ copySourceTree (cleanSource target) newPath
  when (P.strictness (P.params cache) /= P.Lax)
       (do -- Strict mode - download dependencies to depend environment,
           -- sync downloads to build environment and install dependencies there.
           _ <- evalMonadOS (withTmp (downloadDependencies buildTree buildDepends sourceFingerprint)) dependOS
           when (not noClean) (evalMonadOS (syncOS (EnvRoot buildRoot)) dependOS >> return ())
           _ <- evalMonadOS (installDependencies buildTree buildDepends sourceFingerprint) buildOS
           return ())
  return buildTree

    where
      noClean = P.noClean (P.params cache)
      buildDepends = P.buildDepends (P.params cache)

-- | Get the control info for the newest version of a source package
-- available in a release.  Make sure that the files for this build
-- architecture are available.  FIXME: Note that this should *not*
-- find packages which are simply installed in the environment but not
-- available from the repositories listed in sources.list (but
-- currently it does.)
getReleaseControlInfo :: (MonadOS m, MonadRepos m) => Target -> m (Maybe SourcePackage, SourcePackageStatus, String)
getReleaseControlInfo target = do
  sourcePackages' <- (sortBy compareVersion . sortSourcePackages [packageName]) <$> osSourcePackages
  binaryPackages' <- sortBinaryPackages (nub . concat . map sourcePackageBinaryNames $ sourcePackages') <$> osBinaryPackages
  let sourcePackagesWithBinaryNames = zip sourcePackages' (map sourcePackageBinaryNames sourcePackages')
      message status =
          intercalate "\n"
                  (["  Source Package Versions: " ++ show (map (second prettyDebianVersion . sourcePackageVersion) sourcePackages'),
                    "  Required Binary Package Names:"] ++
                   map (("   " ++) . show) (zip (map (second prettyDebianVersion . sourcePackageVersion) sourcePackages') (map sourcePackageBinaryNames sourcePackages')) ++
                   missingMessage status ++
                   ["  Binary Package Versions: " ++ show (map (second prettyDebianVersion . binaryPackageVersion) binaryPackages'),
                    "  Available Binary Packages of Source Package:"] ++
                   map (("   " ++) . show) (zip (map (second prettyDebianVersion . sourcePackageVersion) sourcePackages') (map (availableDebNames binaryPackages') sourcePackages')))
  return $ case zip sourcePackages' (map (isComplete binaryPackages') sourcePackagesWithBinaryNames) of
    (info, status@Complete) : _ -> (Just info, All, message status)
    (info, status@(Missing missing)) : _ -> (Just info, Indep missing, message status)
    _ -> (Nothing, None, message Complete)
    where
      packageName = G.sourceName (targetDepends target)
      missingMessage Complete = []
      missingMessage (Missing missing) = ["  Missing Binary Package Names: "] ++ map (\ p -> "   " ++ unBinPkgName p) missing

      sourcePackageVersion package =
          case ((fieldValue "Package" . sourceParagraph $ package), (fieldValue "Version" . sourceParagraph $ package)) of
            (Just name, Just version) -> (T.unpack name, parseDebianVersion (T.unpack version))
            _ -> error "Missing Package or Version field"
      binaryPackageVersion package =
          case ((fieldValue "Package" . packageInfo $ package), (fieldValue "Version" . packageInfo $ package)) of
            (Just name, Just version) -> (BinPkgName (T.unpack name), parseDebianVersion (T.unpack version))
            _ -> error "Missing Package or Version field"
      compareVersion a b = case ((fieldValue "Version" . sourceParagraph $ a), (fieldValue "Version" . sourceParagraph $ b)) of
                             (Just a', Just b') -> compare (parseDebianVersion . T.unpack $ b') (parseDebianVersion . T.unpack $ a')
                             _ -> error "Missing Version field"
      -- The source package is complete if the correct versions of the
      -- required binary packages are all available, either as debs or
      -- udebs.  Because it is easier to check for available debs, we
      -- do that first and only check for udebs if some names are missing.
      isComplete :: [BinaryPackage] -> (SourcePackage, [BinPkgName]) -> Status
      isComplete binaryPackages (sourcePackage, requiredBinaryNames) =
          if Set.difference missingDebs udebs == Set.empty {- && (unableToCheckUDebs || missingUdebs == Set.empty) -}
          then Complete
          else Missing (Set.toList missingDebs ++ Set.toList missingUdebs)
          where
            (_readyDebs, missingDebs) = Set.partition (`Set.member` availableDebs) required
            (_readyUdebs, missingUdebs) =
                if unableToCheckUDebs
                then (Set.empty, Set.empty)
                else Set.partition (`Set.member` (Set.union availableDebs availableUDebs)) required
            required = Set.fromList requiredBinaryNames
            -- Which binary packages produced from this source package are available?
            availableDebs = Set.fromList (availableDebNames binaryPackages sourcePackage)
            availableUDebs = Set.fromList (availableUDebNames sourcePackage)
      udebs :: Set.Set BinPkgName
      udebs = foldr collect Set.empty (T.flags (download (tgt target)))
      collect :: P.PackageFlag -> Set.Set BinPkgName -> Set.Set BinPkgName
      collect (P.UDeb name) udebs = Set.insert (BinPkgName name) udebs
      collect _ udebs = udebs
      -- A binary package is available either if it appears in the
      -- package index, or if it is an available udeb.
      availableDebNames :: [BinaryPackage] -> SourcePackage -> [BinPkgName]
      availableDebNames binaryPackages sourcePackage =
          map fst . map binaryPackageVersion . filter checkSourceVersion $ binaryPackages
          where checkSourceVersion binaryPackage = maybe False ((==) sourceVersion) (binaryPackageSourceVersion binaryPackage)
                sourceVersion = sourcePackageVersion sourcePackage
      --  or (if it is a udeb) if it simply exists on the
      -- server and has the correct filename.  There is no way to
      -- decide whether a package is a udeb from the package indexes.
      unableToCheckUDebs = True
      availableUDebNames :: SourcePackage -> [BinPkgName]
      availableUDebNames _sourcePackage = (error "availableUDebNames")

data Status = Complete | Missing [BinPkgName]

-- |Compute a new version number for a package by adding a vendor tag
-- with a number sufficiently high to trump the newest version in the
-- dist, and distinct from versions in any other dist.
computeNewVersion :: (MonadApt m, MonadRepos m, MonadOS m) => P.CacheRec -> Target -> Maybe SourcePackage -> SourcePackageStatus -> m (Failing DebianVersion)
computeNewVersion cache target releaseControlInfo _releaseStatus = do
  let current = if buildTrumped then Nothing else releaseControlInfo
      currentVersion = maybe Nothing (Just . parseDebianVersion . T.unpack) (maybe Nothing (fieldValue "Version" . sourceParagraph) current)
      checkVersion :: DebianVersion -> Failing DebianVersion
      checkVersion result =
          maybe (Success result)
                (\ v -> if result <= v
                        then Failure ["Autobuilder bug: new version number " ++ show (prettyDebianVersion result) ++ " is not newer than current version number " ++ show (prettyDebianVersion v)]
                        else Success result)
                currentVersion
  case P.doNotChangeVersion (P.params cache) of
      True -> return (Success sourceVersion)
      False -> do
        let vendor = P.vendorTag (P.params cache)
            oldVendors = P.oldVendorTags (P.params cache)
            release = if (P.isDevelopmentRelease (P.params cache)) then
                          Nothing else
                          (Just (relName (P.baseRelease (P.params cache))))
            extra = P.extraReleaseTag (P.params cache)
            aliases = \ x -> maybe x id (lookup x (P.releaseAliases (P.params cache)))
          {-
              aliases = f
                  where
                    f x = case lookup x (P.releaseAliases (P.params cache)) of
                            Nothing -> x
                            Just x' -> if x == x' then x else f x' in
           -}
        -- All the versions that exist in the pool in any dist,
        -- the new version number must not equal any of these.
        available <- sortSourcePackages [G.sourceName (targetDepends target)] <$> aptSourcePackages
        qPutStrLn ("available versions: " ++ show available)
        case parseTag (vendor : oldVendors) sourceVersion of
          (_, Just tag) -> return $
                             Failure ["Error: the version string in the changelog has a vendor tag (" ++ show tag ++
                                      ".)  This is prohibited because the autobuilder needs to fully control suffixes" ++
                                      " of this form.  This makes it difficult for the author to know what version" ++
                                      " needs to go into debian/changelog to trigger a build by the autobuilder," ++
                                      " particularly since each distribution may have different auto-generated versions."]
          (_, Nothing) -> do
              let newVersion = setTag aliases vendor oldVendors release extra currentVersion (catMaybes . map getVersion $ available) sourceVersion
              qPutStrLn ("new version: " ++ show newVersion)
              return $ newVersion >>= checkVersion
    where
      -- Version number in the changelog entry of the checked-out
      -- source code.  The new version must also be newer than this.
      sourceVersion = logVersion sourceLog
      sourceLog = entry . cleanSource $ target
      getVersion paragraph =
          maybe Nothing (Just . parseDebianVersion . T.unpack) (fieldValue "Version" . sourceParagraph $ paragraph)
      buildTrumped = elem (debianSourcePackageName target) (P.buildTrumped (P.params cache))

-- | Return the first build dependency solution if it can be computed.
-- The actual list could be arbitrarily long, this prevents the caller
-- from trying to look at it.
buildDepSolution :: (MonadOS m, MonadIO m, MonadRepos m, HasDebianControl control) => Arch -> [BinPkgName] -> control -> m (Failing [BinaryPackage])
buildDepSolution arch preferred target = do
  solns <- buildDepSolutions arch preferred target
  return $ case solns of
             Success ((_count, deps) : _) -> Success deps
             Failure x -> Failure x
             _ -> Failure [$(symbol 'buildDepSolution) ++ ": Internal error 4"]

-- FIXME: Most of this code should move into Debian.Repo.Dependencies
buildDepSolutions :: (MonadOS m, MonadIO m, MonadRepos m, HasDebianControl control) => Arch -> [BinPkgName] -> control -> m (Failing [(Int, [BinaryPackage])])
buildDepSolutions arch preferred target =
    do globalBuildDeps <- buildEssential
       packages <- osBinaryPackages
       let info = G.buildDependencies target
       -- q12 "Searching for build dependency solution" $
       -- We don't discard any dependencies here even if they are
       -- mentioned in Relax-Depends, that only applies to deciding
       -- whether to build, once we are building we need to install all
       -- the dependencies.  Hence this empty list.
       let relations' = G.relations info ++ globalBuildDeps
           relations'' = simplifyRelations packages relations' preferred arch
       -- Do not stare directly into the solutions!  Your head will
       -- explode (because there may be a lot of them.)  Also, this
       -- will be slow if solutions is not compiled.
       case Debian.Repo.Dependencies.solutions packages (filter (not . alwaysSatisfied) relations'') 100000 of
         Left error -> return $ Failure [error, message relations' relations'']
         Right solutions -> return $ Success solutions
    where
      alwaysSatisfied xs = any isNothing xs && all isNothing xs
      message relations' relations'' =
          "Build dependency relations:\n " ++
          concat (intersperse "\n " (map (\ (a, b) -> show (map pretty a) ++ " -> " ++ show (map prettySimpleRelation b))
                                              (zip relations' relations'')))

-- In ghc610, using readFile on pseudo files in /proc hangs.  Use this instead.
--rf path = lazyCommand ("cat '" ++ path ++ "'") L.empty >>= return . (\ (o, _, _) -> o) . collectOutputUnpacked

parseProcCpuinfo :: IO [(String, String)]
parseProcCpuinfo =
    readFile "/proc/cpuinfo" >>= return . map makePair . catMaybes . map (matchRegex re) . lines
    where
      re = mkRegex "^(.*[^ \t])[ \t]*:[ \t]*([^ \t].*)$"
      makePair [a, b] = (a, b)
      makePair _ = error "error parsing /proc/cpuinfo"

parseProcMeminfo :: IO [(String, String)]
parseProcMeminfo =
    readFile "/proc/meminfo" >>= return . map makePair . catMaybes . map (matchRegex re) . lines
    where
      re = mkRegex "^(.*[^ \t])[ \t]*:[ \t]*([^ \t].*)$"
      makePair [a, b] = (a, b)
      makePair _ = error "error parsing /proc/meminfo"

lookupAll :: Eq a => a -> [(a, b)] -> [b]
lookupAll _ [] = []
lookupAll a ((a', b) : pairs) | a == a' = b : (lookupAll a pairs)
lookupAll a (_ : pairs) = lookupAll a pairs

-- |Add Build-Info field to the .changes file
updateChangesFile :: NominalDiffTime -> ChangesFile -> IO ChangesFile
updateChangesFile elapsed changes = do
  qPutStrLn "Updating changes file"
  do  let (Paragraph fields) = changeInfo changes
{-    autobuilderVersion <- processOutput "dpkg -s autobuilder | sed -n 's/^Version: //p'" >>=
                            return . either (const Nothing) Just >>=
                            return . maybe Nothing (listToMaybe . lines) -}
      hostname <- let p = shell "hostname" in readProc p "" >>= collectProcessOutput' p >>= return . listToMaybe . lines . L.unpack
      cpuInfo <- parseProcCpuinfo
      memInfo <- parseProcMeminfo
      machine <- let p = shell "uname -m" in readProc p "" >>= collectProcessOutput' p >>= return . listToMaybe . lines . L.unpack
      date <- getCurrentLocalRFC822Time
      let buildInfo = ["Autobuilder-Version: " ++ V.autoBuilderVersion] ++
                      ["Time: " ++ show elapsed] ++
                      ["Date: " ++ show date] ++
                      maybeField "Memory: " (lookup "MemTotal" memInfo) ++
                      maybeField "CPU: " (lookup "model name" cpuInfo) ++
                      ["CPU count: " ++ (show . length . lookupAll "processor" $ cpuInfo)] ++
                      maybeField "OS Architecture: " machine ++
                      maybeField "CPU MHz: " (lookup "cpu MHz" cpuInfo) ++
                      maybeField "CPU cache: " (lookup "cache size" cpuInfo) ++
                      maybeField "Host: " hostname
      let fields' = sinkFields (== "Files")
                    (Paragraph $ fields ++ [Field ("Build-Info", T.pack ("\n " <> intercalate "\n " buildInfo))])
      -- let changes' = changes {changeInfo = Paragraph fields'}
      -- replaceFile (Debian.Repo.path changes') (show (Control [fields']))
      return changes {changeInfo = fields'}
    where
      maybeField tag value = maybe [] ((: []) . (tag ++)) value

-- |Move this to {-Debian.-} Control
sinkFields :: (Eq a) => (a -> Bool) -> Paragraph' a -> Paragraph' a
sinkFields f (Paragraph fields) =
    let (a, b) = partition f' fields in Paragraph (b ++ a)
    where f' (Field (name, _)) = f name
          f' (Comment _) = False

-- |Download the package's build dependencies into /var/cache
downloadDependencies :: (MonadOS m, MonadIO m, MonadCatch m, MonadMask m) => DebianBuildTree -> [String] -> Fingerprint -> m String
downloadDependencies = buildDependencies True

pathBelow :: FilePath -> FilePath -> FilePath
pathBelow root path =
    maybe (error message) id (dropPrefix root path)
    where message = "Expected a path below " ++ root ++ ", saw " ++ path

-- |Install the package's build dependencies.
installDependencies :: (MonadOS m, MonadCatch m, MonadIO m, MonadMask m) => DebianBuildTree -> [String] -> Fingerprint -> m String
installDependencies = buildDependencies False

buildDependencies :: (MonadOS m, MonadCatch m, MonadIO m, MonadMask m) => Bool -> DebianBuildTree -> [String] -> Fingerprint -> m String
buildDependencies downloadOnly source extra sourceFingerprint =
    do root <- rootPath . osRoot <$> getOS
       let path = pathBelow root (topdir source)
           -- pbuilderCommand = "cd '" ++  path ++ "' && /usr/lib/pbuilder/pbuilder-satisfydepends"
           pbuilderCommand = (proc "/usr/lib/pbuilder/pbuilder-satisfydepends" []) {cwd = Just path}
           -- aptGetCommand = "apt-get --yes --force-yes install -o APT::Install-Recommends=True --download-only " ++ intercalate " " (showDependencies' sourceFingerprint ++ extra)
           aptGetCommand = proc "apt-get" (["--yes", "--force-yes", "install", "-o", "APT::Install-Recommends=True"] ++
                                           (if downloadOnly then ["--download-only"] else []) ++
                                           showDependencies' sourceFingerprint ++ extra)
           -- command = ("export DEBIAN_FRONTEND=noninteractive; " ++ (if True then aptGetCommand else pbuilderCommand))
           command = (if True then aptGetCommand else pbuilderCommand) {env = Just [("DEBIAN_FRONTEND", "noninteractive")]}
       if downloadOnly then (qPutStrLn $ "Dependency packages:\n " ++ intercalate "\n  " (showDependencies' sourceFingerprint)) else return ()
       qPutStrLn $ (if downloadOnly then "Downloading" else "Installing") ++ " build dependencies into " ++ root
       out <- withProc (liftIO (useEnv' root forceList (noisier 2 (readProcFailing command "")) >>=
                                collectOutputAndError' command))
       return $ decode out

-- | This should probably be what the real useEnv does.
useEnv' :: FilePath -> (a -> IO a) -> IO a -> IO a
useEnv' rootPath force action = quieter 1 $ useEnv rootPath force $ noisier 1 action

-- |Set a "Revision" line in the .dsc file, and update the .changes
-- file to reflect the .dsc file's new md5sum.  By using our newdist
-- program to update the pool, this line from the .dsc file is then
-- included in the package's entry in the Sources.gz file.  Then we
-- can compare the revision from the uploaded package with the current
-- TLA revision to decide whether to build.
setRevisionInfo :: Fingerprint -> ChangesFile -> IO ChangesFile
setRevisionInfo fingerprint changes =
    case partition (isSuffixOf ".dsc" . changedFileName) (changeFiles changes) of
      ([file], otherFiles) -> do
            qPutStrLn ("Setting revision field in " <> changedFileName file)
            let dscFilePath = changeDir changes </> changedFileName file
            newDscFile <- parseControlFromFile dscFilePath >>= return . either (error . show) addField
            replaceFile dscFilePath (show (pretty newDscFile))
            md5 <- md5sum dscFilePath
            sha1 <- sha1sum dscFilePath
            sha256 <- sha256sum dscFilePath
            case (md5, sha1, sha256) of
              (Success md5, Success sha1, Success sha256) ->
                  do
                    size <- getFileStatus dscFilePath >>= return . fileSize
                    let changes' = changes {changeFiles = (otherFiles ++ [file {changedFileMD5sum = md5, changedFileSHA1sum = sha1, changedFileSHA256sum = sha256, changedFileSize = size}])}
                    saveChangesFile changes'
                    return changes'
              e -> error (show e)
      -- A binary only build will have no .dsc file
      ([], _) -> return changes
      (several, _) -> error ("Multiple .dsc files found in source package: " ++ intercalate ", " (map (show . pretty) several))
    where
      addField (Control (Paragraph sourceInfo : binaryInfo)) =
          Control (newSourceInfo : binaryInfo)
          where newSourceInfo = raiseFields (/= "Files") (Paragraph (sourceInfo ++ [Field ("Fingerprint", " " ++ showFingerprint fingerprint)]))
      addField (Control []) = error "Invalid control file"

-- | Run a checksum command on a file, return the resulting checksum as text.
doChecksum :: String -> (String -> String) -> FilePath -> IO (Failing String)
doChecksum cmd f path =
    doChecksum' `catch` (\ (e :: IOError) -> return (Failure ["Error running " ++ cmd'' ++ ": " ++ show e]))
    where
      doChecksum' =
          do result <- readProcessWithExitCode cmd' [path] ""
             case result of
               (ExitFailure n, _, _) -> return $ Failure ["Error " ++ show n ++ " running " ++ cmd'']
               (_, out, _) -> return (Success (f out))
      cmd' = "/usr/bin/" ++ cmd
      cmd'' = showCommandForUser cmd' [path]

md5sum :: FilePath -> IO (Failing String)
md5sum = doChecksum "md5sum" (take 32)
sha1sum :: FilePath -> IO (Failing String)
sha1sum = doChecksum "sha1sum" (take 40)
sha256sum :: FilePath -> IO (Failing String)
sha256sum = doChecksum "sha256sum" (take 64)

forceList :: [a] -> IO [a]
forceList output = evaluate (length output) >> return output
