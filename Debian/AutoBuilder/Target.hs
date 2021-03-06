{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS -Wall -fwarn-unused-imports -fno-warn-name-shadowing -fno-warn-orphans #-}
-- |A Target represents a particular set of source code and the
-- methods to retrieve and update it.
-- 
-- Author: David Fox <ddssff@gmail.com>
module Debian.AutoBuilder.Target
    ( changelogText     -- Tgt -> Maybe String -> [PkgVersion] -> String
    , buildTargets
    , showTargets
    , decode
    ) where

import Control.Applicative.Error (Failing(..))
import Control.Arrow (second)
import Control.Exception (AsyncException(UserInterrupt), Exception, fromException, SomeException, throw, toException)
import Control.Lens (to, view)
import Control.Monad.Catch (bracket, catch, MonadMask, try)
import Control.Monad.Except (MonadError)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Control.Monad.RWS (liftIO, MonadIO, when)
import Control.Monad.State (MonadState)
import qualified Data.ByteString.Char8 as B (concat)
import qualified Data.ByteString.Lazy.Char8 as L (ByteString, empty, toChunks, unpack)
import qualified Data.ByteString.UTF8 as UTF8 (toString)
import Data.Either (partitionEithers)
import Data.Function (on)
import Data.List as List (intercalate, intersect, intersperse, isSuffixOf, nub, partition, sortBy, lookup, null)
import Data.Map as Map (Map, fromList, lookup)
import Data.Maybe (catMaybes, fromJust, isNothing, listToMaybe, isJust)
import Data.Monoid ((<>))
import Data.Set as Set (difference, empty, fromList, insert, member, null, Set, size, toList, union)
import qualified Data.Text as T (pack, unpack)
import Data.Time (NominalDiffTime)
import Debian.Arch (Arch)
import qualified Debian.AutoBuilder.Params as P (baseRelease, isDevelopmentRelease)
import Debian.AutoBuilder.Types.Buildable (Buildable(..), failing, prepareTarget, relaxDepends, Target(tgt, cleanSource, targetControl), targetRelaxed)
import qualified Debian.AutoBuilder.Types.CacheRec as P (CacheRec(params))
import qualified Debian.AutoBuilder.Types.Download as T (Download(buildWrapper, getTop, logText), method)
import Debian.AutoBuilder.Types.Fingerprint (ReleaseControlInfo(..), buildDecision, targetFingerprint)
import qualified Debian.AutoBuilder.Types.Packages as P (PackageFlag)
import qualified Debian.AutoBuilder.Types.ParamRec as P (ParamRec(autobuilderEmail, buildDepends, buildRelease, buildTrumped, discard, doNotChangeVersion, dryRun, extraReleaseTag, noClean, oldVendorTags, preferred, releaseAliases, setEnv, strictness, vendorTag), Strictness(Lax))
import qualified Debian.AutoBuilder.Version as V (autoBuilderVersion)
import Debian.Changes (ChangedFileSpec(changedFileSize, changedFileName, changedFileMD5sum, changedFileSHA1sum, changedFileSHA256sum), ChangeLogEntry(logWho, logVersion, logDists, logDate, logComments), ChangesFile(changeRelease, changeInfo, changeFiles, changeDir))
import Debian.Codename (codename)
import Debian.Control (Control'(Control), ControlFunctions(parseControlFromFile), Field'(Comment, Field), fieldValue, Paragraph'(..), raiseFields, HasDebianControl, debianSourcePackageName)
import Debian.Debianize (CabalInfo)
import qualified Debian.GenBuildDeps as G
import Debian.Pretty (prettyShow, ppShow, ppPrint)
import Debian.Relation (BinPkgName(..), SrcPkgName(..))
import Debian.Relation.ByteString (Relation(..), Relations)
import Debian.Repo.AptKey (AptKey, aptKey, MonadApt)
import Debian.Repo.Changes (saveChangesFile)
import Debian.Repo.Dependencies (prettySimpleRelation, simplifyRelations, solutions)
import Debian.Repo.Fingerprint (RetrieveMethod, dependencyChanges, DownstreamFingerprint, Fingerprint, packageFingerprint, showDependencies', showFingerprint)
import Debian.Repo.LocalRepository (LocalRepository, uploadLocal)
import Debian.Repo.MonadOS (MonadOS, getOS, evalMonadOS, buildEssential, syncOS)
import Debian.Repo.MonadRepos (HasReposState, MonadRepos)
import Debian.Repo.OSImage (osRoot)
import Debian.Repo.OSKey (HasOSKey, OSKey(_root))
import Debian.Repo.Package (binaryPackageSourceVersion, sourcePackageBinaryNames)
import Debian.Repo.PackageID (PackageID(packageVersion))
import Debian.Repo.PackageIndex (BinaryPackage(packageInfo), SourcePackage(sourceParagraph, sourcePackageID), sortBinaryPackages, sortSourcePackages{-, prettyBinaryPackage-})
import Debian.Repo.Prelude (symbol)
import Debian.Repo.Rsync (HasRsyncError)
import Debian.Repo.SourceTree (addLogEntry, buildDebs, copySourceTree, DebianBuildTree, findChanges, findOneDebianBuildTree, SourcePackageStatus(..), BuildDecision(..), HasChangeLog(entry), HasDebDir(debdir), HasTopDir(topdir))
import Debian.Repo.State.AptImage (aptSourcePackages)
import Debian.Repo.State.OSImage (osSourcePackages, osBinaryPackages, updateOS, buildArchOfOS)
import Debian.Repo.State.Package (scanIncoming, InstallResult(Ok), showErrors, evalInstall)
import Debian.Repo.Top (MonadTop, TopDir, toTop)
import Debian.TH (here)
import Debian.Time (getCurrentLocalRFC822Time)
import Debian.URI (HasParseError)
import Debian.Version (DebianVersion, parseDebianVersion', prettyDebianVersion)
import Debian.VersionPolicy (parseTag, setTag)
import Extra.EnvPath (rootPath)
import Extra.Except
import Extra.Files (replaceFile)
import Extra.List (dropPrefix)
import Extra.Misc (columns)
import Extra.Process (modifyProcessEnv, runV2, runVE2)
import Extra.Verbosity (ePutStrLn, noisier, qPutStrLn, quieter, ePutStr)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, getCurrentDirectory, removeDirectory, setCurrentDirectory)
import System.Exit (ExitCode(ExitSuccess, ExitFailure), exitWith)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import System.Posix.Files (fileSize, getFileStatus)
import System.Process (CreateProcess(cwd), proc, readProcessWithExitCode, shell, showCommandForUser, readProcess)
import System.Process.ListLike (showCreateProcessForUser)
import System.Unix.Chroot (useEnv)
import Debian.Repo.Mount (withProcAndSys, withTmp)
import Text.Printf (printf)
import "regex-compat-tdfa" Text.Regex (matchRegex, mkRegex)

instance T.Download a => Ord (Target a) where
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
changelogText :: T.Download a => Buildable a -> Maybe DownstreamFingerprint -> Fingerprint -> String
changelogText buildable old new = ("  * " ++ T.logText (download buildable) ++ "\n" ++ dependencyChanges old new ++ "\n")

-- |Generate the string of build dependency versions:
-- package1=version1 package2=version2 ...
_formatVersions :: [PackageID BinPkgName] -> String
_formatVersions buildDeps =
    prefix ++
    intercalate prefix (map (show . prettySimpleRelation . Just) buildDeps) ++
    "\n"
    where prefix = "\n    "

prepareTargets ::
    forall r s e m a. (MonadIOError e m, HasLoc e, MonadMask m, HasRsyncError e, MonadOS r s m, T.Download a)
    => P.CacheRec -> [Buildable a] -> m [Target a]
prepareTargets cache targetSpecs =
    do results <- mapM (prepare (length targetSpecs)) (zip [1..] targetSpecs)
       let (failures, targets) = partitionEithers results
       let msg = "Could not prepare " ++ show (length failures) ++ " targets:\n" ++
                 concatMap (\ (n, e) -> printf "%4d. " n ++ show e ++ "\n") (zip [(1::Int)..] failures)
       case List.null failures of
         True -> return targets
         False -> ePutStr msg >> error msg
    where
      prepare :: Int -> (Int, Buildable a) -> m (Either SomeException (Target a))
      prepare count (index, tgt) =
          do qPutStrLn (printf "[%2d of %2d] %s in %s" index count (ppShow . debianSourcePackageName $ tgt) (T.getTop $ download $ tgt))
             try (prepareTarget cache tgt) >>=
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
buildTargets :: (MonadIOError e m, HasLoc e, Exception e, HasRsyncError e, HasParseError e, MonadMask m,
                 HasReposState s, MonadState s m,
                 MonadTop r m, MonadApt r m, T.Download a) =>
                P.CacheRec -> OSKey -> OSKey -> LocalRepository -> [Buildable a] -> m (LocalRepository, [Target a])
buildTargets _ _ _ localRepo [] = return (localRepo, [])
buildTargets cache dependOS buildOS localRepo !targetSpecs =
    do qPutStrLn ("\nAssembling source trees: (" ++ $(symbol 'buildTargets) ++ ")\n")
       targets <- evalMonadOS (prepareTargets cache targetSpecs) dependOS
       qPutStrLn "\nBuilding all targets:"
       failed <- buildLoop cache localRepo dependOS buildOS targets
       return (localRepo, failed)

-- Execute the target build loop until all the goals (or everything) is built
-- FIXME: Use sets instead of lists
buildLoop :: forall r s e m a. (MonadIOError e m, HasLoc e, MonadMask m, Exception e, HasRsyncError e, HasParseError e, MonadRepos s m, MonadTop r m, MonadApt r m, T.Download a) =>
             P.CacheRec -> LocalRepository -> OSKey -> OSKey -> [Target a] -> m [Target a]
buildLoop cache localRepo dependOS buildOS !targets =
    Set.toList <$> loop (Set.fromList targets) Set.empty
    where
      -- This loop computes the list of known ready targets and call
      -- loop2 to build them
      loop :: Set.Set (Target a) -> Set.Set (Target a) -> m (Set.Set (Target a))
      loop unbuilt failed | Set.null unbuilt = return failed
      loop unbuilt failed =
          ePutStr "\nComputing ready targets..." >>
          evalMonadOS buildEssential dependOS >>= \ globalBuildDeps ->
          case computeReadyTargets cache globalBuildDeps (Set.toList unbuilt) of
            [] -> ePutStrLn "done\n" >> return failed
            ready ->
                do ePutStrLn ("\n" <> makeTable ready)
                   loop2 (Set.difference unbuilt (Set.fromList $ map G.ready ready)) failed ready
      loop2 :: Set.Set (Target a) -- unbuilt: targets which have not been built and are not ready to build
            -> Set.Set (Target a) -- failed: Targets which either failed to build or were blocked by a target that failed to build
            -> [G.ReadyTarget (Target a)] -- ready: the list of known buildable targets
            -> m (Set.Set (Target a))
      loop2 unbuilt failed [] =
          -- Out of ready targets, re-do the dependency computation
          loop unbuilt failed
      loop2 unbuilt failed (G.ReadyTarget {G.ready = target, G.waiting = blocked} : ready') =
          do ePutStrLn (printf "[%2d of %2d] TARGET: %s - %s"
                        (length targets - (Set.size unbuilt + length ready')) (length targets) (ppShow . debianSourcePackageName $ target) (show (T.method (download (tgt target)))))
             -- Build one target.
             result <- if Set.member (debianSourcePackageName target) (P.discard (P.params cache))
                       then return (Failure ["--discard option set"])
                       else (Success <$> buildTarget cache dependOS buildOS localRepo target) `catch` handleBuildException
             failing -- On failure the target and its dependencies get
                     -- added to failed.
                     (\ errs ->
                          do ePutStrLn ("Package build failed:\n " ++ intercalate "\n " errs ++ "\n" ++
                                        "Discarding " ++ ppShow (debianSourcePackageName target) ++ " and its dependencies:\n  " ++
                                        concat (intersperse "\n  " (map (ppShow . debianSourcePackageName) blocked)))
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
      handleBuildException :: SomeException -> m (Failing (Maybe LocalRepository))
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

makeTable :: T.Download a => [G.ReadyTarget (Target a)] -> String
makeTable ready =
    unlines . map (intercalate " ") . columns $ goalsLine ++ [[""]] ++ readyLines
    where
      goalsLine = []
      readyLines = map readyLine ready
      readyLine (G.ReadyTarget {G.ready = ready, G.waiting = blocked}) =
          [" Ready:", ppShow (debianSourcePackageName ready) {-++ " (" ++ intercalate ", " (map ppShow (G.binaryNames ready)) ++ ")"-}, "Blocking " ++ show (length blocked) ++ ": [" ++ intercalate ", " (map (ppShow . debianSourcePackageName) blocked) ++ "]"]

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
computeReadyTargets :: T.Download a => P.CacheRec -> Relations -> [Target a] -> [G.ReadyTarget (Target a)]
computeReadyTargets cache globalBuildDeps targets =
    -- q12 "Choosing next target" $
    -- Compute the list of build dependency groups, each of which
    -- starts with a target that is ready to build followed by
    -- targets which are blocked by the first target.
    case G.buildable relaxedTarget targets of
      (G.CycleInfo arcs) -> error (cycleMessage cache globalBuildDeps arcs)
      info ->
          case sortBy compareReady . G.readyTargets $ info of
            [] -> []
            ready -> ready
    where
      relaxedTarget :: T.Download a => Target a -> G.DepInfo
      relaxedTarget t = targetRelaxed globalBuildDeps (relaxDepends cache (tgt t)) t
      -- relaxedTargets = map (\ t -> targetRelaxed (relaxDepends cache (tgt t)) t) targets
      -- We choose the next target using the relaxed dependency set
      -- depends :: G.DepInfo -> G.DepInfo -> Ordering
      -- depends target1 target2 = G.compareSource target1 target2
      -- Choose the next target to build.  Look for targets which are
      -- in the goal list, or which block packages in the goal list.
      -- Among those, prefer the target which blocks the most
      -- packages.  If there are goal targets but none of them are
      -- ready to build or directly block
      -- targets include a goal as readyamongoals none of the
      compareReady :: G.ReadyTarget a ->  G.ReadyTarget a -> Ordering
      -- Prefer targets which block more package
      compareReady = flip (compare `on` (length . G.waiting))

cycleMessage :: T.Download a => P.CacheRec -> Relations -> [(Target a, Target a)] -> String
cycleMessage cache globalBuildDeps arcs =
    "This dependency cycle needs to be broken:\n  " ++
    unlines (map (intercalate " ")
             (columns (["these binary packages", "from this source package", "", "force a rebuild of"] :
                       (map arcTuple arcs)))) ++
    "\nAdd one or more of these lines (but as few as possible) to your configuration file:\n  " ++
    intercalate "\n  " (map relaxLine (nub (concat (map pairs arcs))))
    where
      arcTuple (pkg, dep) =
          let rels = targetRelaxed globalBuildDeps (relaxDepends cache (tgt pkg)) pkg in
          [(show (intersect (binaryNames pkg dep) (binaryNamesOfRelations rels))),
           ppShow (debianSourcePackageName dep), " -> ", ppShow (debianSourcePackageName pkg)]
      relaxLine :: (BinPkgName, SrcPkgName) -> String
      relaxLine (bin, src) = "Relax-Depends: " ++ unBinPkgName bin ++ " " ++ unSrcPkgName src
      pairs :: T.Download a => (Target a, Target a) -> [(BinPkgName, SrcPkgName)]
      pairs (pkg, dep) =
          map (\ bin -> (bin, G.sourceName' (targetControl pkg))) binaryDependencies
              where binaryDependencies = intersect (binaryNames pkg dep) (binaryNamesOfRelations (targetRelaxed globalBuildDeps (relaxDepends cache (tgt pkg)) pkg))
      binaryNamesOfRelations :: G.DepInfo -> [BinPkgName]
      binaryNamesOfRelations info =
          concat (map (map (\ (Rel name _ _) -> name)) (G.relations info))
      binaryNames :: T.Download a => Target a -> Target a -> [BinPkgName]
      binaryNames pkg dep = G.binaryNames (targetRelaxed globalBuildDeps (relaxDepends cache (tgt pkg)) dep)

showTargets :: [(RetrieveMethod, [P.PackageFlag], [CabalInfo -> CabalInfo])] -> String
showTargets targets =
    unlines (heading :
             map (const '-') heading :
             map (\ (count, (spec, _flags, _fns)) -> printf "%4d. " count <> " " <> limit 100 (show spec)) (zip ([1..] :: [Int]) targets)
             -- map concat (columns (reverse (snd (P.foldPackages (\ p (count, rows) -> (count + 1, [printf "%4d. " count, " ", limit 100 (show (P.spec p))] : rows)) targets (1 :: Int, [])))))
            )
    where
      heading = show (length targets) ++ " Targets:"

limit :: Int -> String -> String
limit n s = if length s > n + 3 then take n s ++ "..." else s

qError :: MonadIO m => String -> m b
qError message = qPutStrLn message >> error message

-- Decide whether a target needs to be built and, if so, build it.
buildTarget ::
    forall r s e m a. (MonadIOError e m, HasLoc e, MonadMask m, Exception e, HasRsyncError e, HasParseError e, MonadRepos s m, MonadTop r m, MonadApt r m, T.Download a)
 => P.CacheRec                       -- configuration info
 -> OSKey
 -> OSKey
 -> LocalRepository                  -- ^ The local repository the packages will be uploaded to, this also may already contain packages.
 -> Target a
 -> m (Maybe LocalRepository)   -- The local repository after the upload (if it changed)
buildTarget cache dependOS buildOS repo !target = do
  -- Get the control file from the clean source and compute the
  -- build dependencies
  arch <- evalMonadOS buildArchOfOS dependOS
  -- quieter 2 $ qPutStrLn "Looking for build dependency solutions..."
  -- soln <- evalMonadOS (buildDepSolution arch (map BinPkgName (P.preferred (P.params cache))) target) dependOS
  soln <- evalMonadOS3 (buildDepSolution arch (map BinPkgName (P.preferred (P.params cache))) target) dependOS
  case soln of
        Failure excuses -> qError $ intercalate "\n  " ("Couldn't satisfy build dependencies" : excuses)
        Success packages ->
            do -- quieter 3 $ qPutStrLn ("Build dependency solution: " ++ show (map prettyBinaryPackage packages))
               -- Get the newest available version of a source package,
               -- along with its status, either Indep or All
               info <- evalMonadOS (getReleaseControlInfo target) dependOS
               -- Get the changelog entry from the clean source
               let newFingerprint = targetFingerprint target packages
               -- qPutStrLn "Computing new version number of target package..."
               newVersion <- runReaderT (computeNewVersion cache target info) =<< ((,,) <$> view toTop <*> view aptKey <*> pure dependOS)
               globalBuildDeps <- evalMonadOS buildEssential dependOS
               let decision = buildDecision cache globalBuildDeps target info newFingerprint
               ePutStrLn ("Build decision: " ++ show decision)
               -- qPutStrLn ("newVersion: " ++ show (fmap prettyDebianVersion newVersion))
               -- qPutStrLn ("Release status: " ++ show releaseStatus)
               case newVersion of
                 Failure messages -> qError (intercalate "\n  " ("Failure computing new version number:" : messages))
                 Success version ->
                     -- If we are doing an arch only build, the version number needs to match the
                     -- version number of the architecture independent package already uploaded.
                     let buildVersion = case decision of
                                          Arch _ -> repoVersion info
                                          _ -> Just version in
                     case decision of
                       Error message -> qError ("Failure making build decision: " ++ message)
                       No _ -> return Nothing
                       _ ->  buildPackage cache dependOS buildOS buildVersion (oldFingerprint info) newFingerprint target decision repo >>=
                             return . Just

evalMonadOS3 :: (MonadApt r m, MonadTop r m) => ReaderT (TopDir, AptKey, OSKey) m a -> OSKey -> m a
evalMonadOS3 task os = runReaderT task =<< ((,,) <$> view toTop <*> view aptKey <*> pure os)

repoVersion :: ReleaseControlInfo -> Maybe DebianVersion
repoVersion = fmap (packageVersion . sourcePackageID) . releaseSourcePackage

oldFingerprint :: ReleaseControlInfo -> Maybe DownstreamFingerprint
oldFingerprint = maybe Nothing packageFingerprint . releaseSourcePackage

-- | Build a package and upload it to the local repository.
buildPackage ::
    forall e r s m a. (MonadIOError e m, HasLoc e, MonadMask m, Exception e, HasRsyncError e, HasParseError e,
                       HasReposState s, MonadState s m, MonadTop r m, T.Download a)
    => P.CacheRec
    -> OSKey
    -> OSKey
    -> Maybe DebianVersion
    -> Maybe DownstreamFingerprint
    -> Fingerprint
    -> Target a
    -> BuildDecision
    -> LocalRepository
    -> m LocalRepository
buildPackage cache dependOS buildOS newVersion oldFingerprint newFingerprint !target decision repo = do
  checkDryRun
  source <- noisier 2 $ prepareBuildTree cache dependOS buildOS newFingerprint target
  logEntry source
  result <- withError (withLoc $here) $ evalMonadOS (withProcAndSys [$here] (view (to _root . rootPath) buildOS) $ build source) buildOS
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
      build :: forall r' s' m'. (MonadIOError e m', MonadMask m',
                                 MonadReader r' m', HasOSKey r',
                                 HasReposState s', MonadState s' m') => DebianBuildTree -> {-WithProcAndSys-} m' (DebianBuildTree, NominalDiffTime)
      build buildTree =
          do -- The --commit flag does not appear until dpkg-dev-1.16.1,
             -- so we need to check this version number.  We also
             -- don't want to leave the patches subdirectory here
             -- unless we actually created a patch.
             root <- view (osRoot . to _root . rootPath) <$> getOS
             let path = debdir buildTree
                 path' = fromJust (dropPrefix root path)
             dpkgSource <- modifyProcessEnv [("EDITOR", Just "/bin/true")] (proc "dpkg-source" ["--commit", ".", "autobuilder.diff"])
             let doDpkgSource False = do
                   liftIO $ createDirectoryIfMissing True (path' </> "debian/patches")
                   _ <- runV2 [$here] dpkgSource L.empty
                   exists <- liftIO $ doesFileExist (path' </> "debian/patches/autobuilder.diff")
                   when (not exists) (liftIO $ removeDirectory (path' </> "debian/patches"))
                 doDpkgSource True = runV2 [$here] dpkgSource L.empty >> return ()
                 -- doDpkgSource' = setEnv "EDITOR" "/bin/true" >> readCreateProcess ((proc "dpkg-source" ["--commit", ".", "autobuilder.diff"]) {cwd = Just path'}) L.empty
             _ <- useEnv' root path' (\ _ -> return ())
                             (do -- Get the version number of dpkg-dev in the build environment
                                 let p = shell ("dpkg -s dpkg-dev | sed -n 's/^Version: //p'")
                                 result <- runVE2 [$here] p "" :: m' (Either e (ExitCode, L.ByteString, L.ByteString))
                                 installed <- case result of
                                                Right (ExitSuccess, out, _) -> return . head . words . L.unpack $ out
                                                _ -> error $ showCreateProcessForUser p ++ " -> " ++ show result
                                 -- If it is >= 1.16.1 we may need to run dpkg-source --commit.
                                 result <- runVE2 [$here] (shell ("dpkg --compare-versions '" ++ installed ++ "' ge 1.16.1")) "" :: m' (Either e (ExitCode, L.ByteString, L.ByteString))
                                 newer <- case result of
                                            Right (ExitSuccess, _, _ :: L.ByteString) -> return True
                                            _ -> return False
                                 when newer (liftIO (doesDirectoryExist (path' </> "debian/patches")) >>= doDpkgSource))
             -- If newVersion is set, pass a parameter to cabal-debian
             -- to set the exact version number.
             let ver = maybe [] (\ v -> [("CABALDEBIAN", Just (show ["--deb-version", show (prettyDebianVersion v)]))]) newVersion
             let env = ver ++ P.setEnv (P.params cache)
             let action = buildDebs (P.noClean (P.params cache)) env buildTree decision
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
          where setDist name (Field ("Distribution", _)) = Field ("Distribution", " " <> T.pack (codename name))
                setDist _ other = other
      doLocalUpload :: forall e' s' m'. (Exception e', HasIOException e', HasParseError e', MonadError e' m', MonadIO m', HasReposState s', MonadState s' m') => (ChangesFile, NominalDiffTime) -> m' LocalRepository
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
              [] -> return repo
              _ -> error $ "Local upload failed:\n " ++ showErrors (map snd results)

-- |Prepare the build image by copying the clean image, installing
-- dependencies, and copying the clean source tree.  For a lax build
-- these operations take place in a different order from other types
-- of builds.  For lax: dependencies, then image copy, then source
-- copy.  For other: image copy, then source copy, then dependencies.
prepareBuildTree :: (MonadIOError e m, HasLoc e, MonadMask m, Exception e, HasRsyncError e, MonadTop r m, MonadRepos s m) =>
                    P.CacheRec -> OSKey -> OSKey -> Fingerprint -> Target a -> m DebianBuildTree
prepareBuildTree cache dependOS buildOS sourceFingerprint target = do
  let dependRoot = view (to _root . rootPath) dependOS
      buildRoot = view (to _root . rootPath) buildOS
  let oldPath = topdir . cleanSource $ target
      newPath = buildRoot ++ fromJust (dropPrefix dependRoot oldPath)
  when (P.strictness (P.params cache) == P.Lax)
       (do -- Lax mode - dependencies accumulate in the dependency
           -- environment, sync that to build environment.
           _ <- evalMonadOS (withProcAndSys [$here] (view (to _root . rootPath) dependOS) (installDependencies (cleanSource target) buildDepends sourceFingerprint)) dependOS
           when (not noClean) (evalMonadOS (syncOS buildOS) dependOS)
           return ())
  buildTree <- case noClean of
                 True ->
                     liftIO (findOneDebianBuildTree newPath) >>=
                     maybe (error ("No build tree at " ++ show newPath)) return
                 False ->
                     copySourceTree (cleanSource target) newPath
  when (P.strictness (P.params cache) /= P.Lax)
       (do -- Strict mode - download dependencies to depend environment,
           -- sync downloads to build environment and install dependencies there.
           _ <- evalMonadOS (withTmp [$here] dependRoot (downloadDependencies buildTree buildDepends sourceFingerprint)) dependOS
           when (not noClean) (evalMonadOS (syncOS buildOS) dependOS >> return ())
           _ <- evalMonadOS (installDependencies buildTree buildDepends sourceFingerprint) buildOS
           return ())
  return buildTree

    where
      noClean = P.noClean (P.params cache)
      buildDepends = P.buildDepends (P.params cache)

-- | Get the control info for the newest version of a source package
-- available in a release.  Make sure that the files for this build
-- architecture are available.
getReleaseControlInfo :: forall r s e m a. (MonadIOError e m, HasLoc e, MonadOS r s m) => Target a -> m ReleaseControlInfo
getReleaseControlInfo target = do
  -- The source packages with the specified name, newest first
  (sourcePackages' :: [SourcePackage]) <- (sortSourcePackages (== packageName)) <$> osSourcePackages
  -- Source packages associated with their binary package names
  -- let sourcePackagesWithBinaryNames :: [(SourcePackage, [BinPkgName])]
  --     sourcePackagesWithBinaryNames = zip sourcePackages' (map sourcePackageBinaryNames sourcePackages')
  -- All The binary packages that came from from those source
  -- packages, sorted by name and version.  Although we know what the
  -- names of the binary packages are, it is difficult to be sure
  -- exactly which versions correspond to a source pacakge, because it
  -- is possible to assign a version to a binary deb that is different
  -- from the source package version.
  (binaryPackages' :: [BinaryPackage]) <- sortBinaryPackages (flip elem . nub . concat . map sourcePackageBinaryNames $ sourcePackages') <$> osBinaryPackages
  sourcePackageStatusPairs <- zip sourcePackages' <$> mapM isComplete sourcePackages'
  return $ case sourcePackageStatusPairs of
             (info, status@Complete) : _ -> ReleaseControlInfo (Just info) All (message sourcePackages' binaryPackages' status)
             (info, status@(Missing missing)) : _ -> ReleaseControlInfo (Just info) (Indep missing) (message sourcePackages' binaryPackages' status)
             _ -> ReleaseControlInfo Nothing None (message sourcePackages' binaryPackages' Complete)
{-
  return $ case zip sourcePackages' (map (isComplete sourcePackage binaryPackages') sourcePackagesWithBinaryNames) of
    (info, status@Complete) : _ -> (Just info, All, message sourcePackages' binaryPackages' status)
    (info, status@(Missing missing)) : _ -> (Just info, Indep missing, message sourcePackages' binaryPackages' status)
    _ -> (Nothing, None, message sourcePackages' binaryPackages' Complete)
-}
    where
      message sourcePackages' binaryPackages' status =
          intercalate "\n"
                  (["  Source Package Versions: " ++ show (map (second prettyDebianVersion . sourcePackageVersion) sourcePackages'),
                    "  Required Binary Package Names:"] ++
                   map (("   " ++) . show) (zip (map (second prettyDebianVersion . sourcePackageVersion) sourcePackages') (map sourcePackageBinaryNames sourcePackages')) ++
                   missingMessage status ++
                   ["  Binary Package Versions: " ++ show (map (second prettyDebianVersion . binaryPackageVersion) binaryPackages'),
                    "  Available Binary Packages of Source Package:"] ++
                   map (("   " ++) . show) (zip (map (second prettyDebianVersion . sourcePackageVersion) sourcePackages') (map (availableDebNames binaryPackages') sourcePackages')))
      packageName = G.sourceName' (targetControl target)
      missingMessage Complete = []
      missingMessage (Missing missing) = ["  Missing Binary Package Names: "] ++ map (\ p -> "   " ++ unBinPkgName p) missing

      sourcePackageVersion package =
          case ((fieldValue "Package" . sourceParagraph $ package), (fieldValue "Version" . sourceParagraph $ package)) of
            (Just name, Just version) -> (T.unpack name, parseDebianVersion' (T.unpack version))
            _ -> error "Missing Package or Version field"
      binaryPackageVersion package =
          case ((fieldValue "Package" . packageInfo $ package), (fieldValue "Version" . packageInfo $ package)) of
            (Just name, Just version) -> (BinPkgName (T.unpack name), parseDebianVersion' (T.unpack version))
            _ -> error "Missing Package or Version field"
      -- compareVersion a b = case ((fieldValue "Version" . sourceParagraph $ a), (fieldValue "Version" . sourceParagraph $ b)) of
      --                        (Just a', Just b') -> compare (parseDebianVersion' . T.unpack $ b') (parseDebianVersion . T.unpack $ a')
      --                        _ -> error "Missing Version field"
      -- The source package is complete if the correct versions of the
      -- required binary packages are all available, either as debs or
      -- udebs.  Because it is easier to check for available debs, we
      -- do that first and only check for udebs if some names are missing.
      isComplete :: SourcePackage -> m Status
      isComplete sourcePackage =
          do let requiredDebNames = sourcePackageBinaryNames sourcePackage
             (binaryDebMap :: Map BinPkgName [BinaryPackage]) <- mapM (\ name -> (sortBinaryPackages (== name) <$> osBinaryPackages)) requiredDebNames >>= return . Map.fromList . zip requiredDebNames
             let (_readyDebs, missingDebs) = List.partition (\ name -> isJust (Map.lookup name binaryDebMap)) requiredDebNames
             case List.null missingDebs of
               True -> return Complete
               False -> return $ Missing missingDebs
{-
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
-}
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
      -- unableToCheckUDebs = True
      -- availableUDebNames :: SourcePackage -> [BinPkgName]
      -- availableUDebNames _sourcePackage = (error "availableUDebNames")

data Status = Complete | Missing [BinPkgName]

-- |Compute a new version number for a package by adding a vendor tag
-- with a number sufficiently high to trump the newest version in the
-- dist, and distinct from versions in any other dist.
computeNewVersion :: (MonadIOError e m, HasLoc e, MonadApt r m, MonadRepos s m, T.Download a) =>
                     P.CacheRec -> Target a -> ReleaseControlInfo -> m (Failing DebianVersion)
computeNewVersion cache target info = do
  let current = if buildTrumped then Nothing else releaseSourcePackage info
      currentVersion = maybe Nothing (Just . parseDebianVersion' . T.unpack) (maybe Nothing (fieldValue "Version" . sourceParagraph) current)
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
                          (Just (codename (P.baseRelease (P.params cache))))
            extra = P.extraReleaseTag (P.params cache)
            aliases = \ x -> maybe x id (List.lookup x (P.releaseAliases (P.params cache)))
          {-
              aliases = f
                  where
                    f x = case lookup x (P.releaseAliases (P.params cache)) of
                            Nothing -> x
                            Just x' -> if x == x' then x else f x' in
           -}
        -- All the versions that exist in the pool in any dist,
        -- the new version number must not equal any of these.
        available <- sortSourcePackages (== G.sourceName' (targetControl target)) <$> aptSourcePackages
        -- quieter 3 $ qPutStrLn ("available versions: " ++ show available)
        case parseTag (vendor : oldVendors) sourceVersion of
          (_, Just tag) -> return $
                             Failure ["Error: the version string in the changelog has a vendor tag (" ++ show tag ++
                                      ".)  This is prohibited because the autobuilder needs to fully control suffixes" ++
                                      " of this form.  This makes it difficult for the author to know what version" ++
                                      " needs to go into debian/changelog to trigger a build by the autobuilder," ++
                                      " particularly since each distribution may have different auto-generated versions."]
          (_, Nothing) -> do
              let newVersion = setTag aliases vendor oldVendors release extra currentVersion (catMaybes . map getVersion $ available) sourceVersion
              -- qPutStrLn ("new version: " ++ show newVersion)
              return $ newVersion >>= checkVersion
    where
      -- Version number in the changelog entry of the checked-out
      -- source code.  The new version must also be newer than this.
      sourceVersion = logVersion sourceLog
      sourceLog = entry . cleanSource $ target
      getVersion paragraph =
          maybe Nothing (Just . parseDebianVersion' . T.unpack) (fieldValue "Version" . sourceParagraph $ paragraph)
      buildTrumped = elem (debianSourcePackageName target) (P.buildTrumped (P.params cache))

-- | Return the first build dependency solution if it can be computed.
-- The actual list could be arbitrarily long, this prevents the caller
-- from trying to look at it.
buildDepSolution :: (MonadIOError e m, HasLoc e, MonadOS r s m, HasDebianControl control) => Arch -> [BinPkgName] -> control -> m (Failing [BinaryPackage])
buildDepSolution arch preferred target = do
  solns <- buildDepSolutions arch preferred target
  return $ case solns of
             Success ((_count, deps) : _) -> Success deps
             Failure x -> Failure x
             _ -> Failure [$(symbol 'buildDepSolution) ++ ": Internal error 4"]

-- FIXME: Most of this code should move into Debian.Repo.Dependencies
buildDepSolutions :: (MonadIOError e m, HasLoc e, MonadOS r s m, HasDebianControl control) => Arch -> [BinPkgName] -> control -> m (Failing [(Int, [BinaryPackage])])
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
           relations''' = filter (not . alwaysSatisfied) relations''
       -- Do not stare directly into the solutions!  Your head will
       -- explode (because there may be a lot of them.)  Also, this
       -- will be slow if solutions is not compiled.
       case Debian.Repo.Dependencies.solutions packages relations''' 100000 of
         Left error -> return $ Failure [error, message relations' relations'']
         Right solutions -> return $ Success solutions
    where
      alwaysSatisfied xs = any isNothing xs && all isNothing xs
      message relations' relations'' =
          "Build dependency relations:\n " ++
          concat (intersperse "\n " (map (\ (a, b) -> show (map ppPrint a) ++ " -> " ++ show (map prettySimpleRelation b))
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
      hostname <- let p = shell "hostname" in runV2 [$here] p "" >>= (\ (_, out, _) -> return out) >>= return . listToMaybe . lines . L.unpack
      cpuInfo <- parseProcCpuinfo
      memInfo <- parseProcMeminfo
      machine <- let p = shell "uname -m" in runV2 [$here] p "" >>= (\ (_, out, _) -> return out) >>= return . listToMaybe . lines . L.unpack
      date <- getCurrentLocalRFC822Time
      let buildInfo = ["Autobuilder-Version: " ++ V.autoBuilderVersion] ++
                      ["Time: " ++ show elapsed] ++
                      ["Date: " ++ show date] ++
                      maybeField "Memory: " (List.lookup "MemTotal" memInfo) ++
                      maybeField "CPU: " (List.lookup "model name" cpuInfo) ++
                      ["CPU count: " ++ (show . length . lookupAll "processor" $ cpuInfo)] ++
                      maybeField "OS Architecture: " machine ++
                      maybeField "CPU MHz: " (List.lookup "cpu MHz" cpuInfo) ++
                      maybeField "CPU cache: " (List.lookup "cache size" cpuInfo) ++
                      maybeField "Host: " hostname
      let fields' = sinkFields (== "Files")
                    (Paragraph $ fields ++ [Field ("Build-Info", T.pack ("\n " <> intercalate "\n " buildInfo))])
      -- let changes' = changes {changeInfo = Paragraph fields'}
      -- replaceFile (Debian.Repo.path changes') (show (Control [fields']))
      return changes {changeInfo = fields'}
    where
      maybeField tag value = maybe [] ((: []) . (tag ++)) value

-- |Move this to {-Debian.-} Control
sinkFields :: (a -> Bool) -> Paragraph' a -> Paragraph' a
sinkFields f (Paragraph fields) =
    let (a, b) = List.partition f' fields in Paragraph (b ++ a)
    where f' (Field (name, _)) = f name
          f' (Comment _) = False

-- |Download the package's build dependencies into /var/cache
downloadDependencies :: (MonadIOError e m, HasLoc e, MonadMask m, MonadOS r s m, Exception e) => DebianBuildTree -> [String] -> Fingerprint -> m String
downloadDependencies = buildDependencies True

pathBelow :: FilePath -> FilePath -> FilePath
pathBelow root path =
    maybe (error message) id (dropPrefix root path)
    where message = "Expected a path below " ++ root ++ ", saw " ++ path

-- |Install the package's build dependencies.
installDependencies :: (MonadIOError e m, HasLoc e, MonadMask m, MonadOS r s m, Exception e) => DebianBuildTree -> [String] -> Fingerprint -> m String
installDependencies = buildDependencies False

buildDependencies ::
    forall e' r' s' m'. (MonadIOError e' m', HasLoc e', MonadMask m', Exception e',
                         HasOSKey r', MonadReader r' m',
                         HasReposState s', MonadState s' m')
    => Bool -> DebianBuildTree -> [String] -> Fingerprint -> m' String
buildDependencies downloadOnly source extra sourceFingerprint =
    do root <- view (osRoot . to _root . rootPath) <$> getOS
       let path = pathBelow root (topdir source)
           path' = fromJust (dropPrefix root (topdir source </> debdir source))
           -- pbuilderCommand = "cd '" ++  path ++ "' && /usr/lib/pbuilder/pbuilder-satisfydepends"
           pbuilderCommand = (proc "/usr/lib/pbuilder/pbuilder-satisfydepends" []) {cwd = Just path}
           -- aptGetCommand = "apt-get --yes --force-yes install -o APT::Install-Recommends=True --download-only " ++ intercalate " " (showDependencies' sourceFingerprint ++ extra)
           aptGetCommand = proc "apt-get" (["--yes", "--force-yes", "install", "-o", "APT::Install-Recommends=True"] ++
                                           (if downloadOnly then ["--download-only"] else []) ++
                                           showDependencies' sourceFingerprint ++ extra)
           -- command = ("export DEBIAN_FRONTEND=noninteractive; " ++ (if True then aptGetCommand else pbuilderCommand))
       command <- liftIO $ modifyProcessEnv [("DEBIAN_FRONTEND", Just "noninteractive")] (if True then aptGetCommand else pbuilderCommand)
       if downloadOnly then (qPutStrLn $ "Dependency packages:\n " ++ intercalate "\n  " (showDependencies' sourceFingerprint)) else return ()
       qPutStrLn $ (if downloadOnly then "Downloading" else "Installing") ++ " build dependencies into " ++ root
       (result :: Either e' (ExitCode, L.ByteString, L.ByteString)) <- useEnv' root path' return (noisier 2 $ runVE2 [$here] command mempty)
       case result of
         Right (ExitSuccess, out :: L.ByteString, _) -> return $ decode out
         _ -> error $ "buildDependencies: " ++ showCreateProcessForUser command ++ " -> " ++ show result

withCurrentDirectory ::
    (MonadIO m, MonadMask m)
    => FilePath  -- ^ Directory to execute in
    -> m a      -- ^ Action to be executed
    -> m a
withCurrentDirectory dir action =
  bracket (liftIO getCurrentDirectory) (liftIO . setCurrentDirectory) $ \ _ -> do
    liftIO (setCurrentDirectory dir)
    action

useEnv' :: (MonadIOError e m, HasLoc e, MonadMask m) => FilePath -> FilePath -> (a -> m a) -> m a -> m a
useEnv' rootPath cwdPath force action =
    quieter 1 $
      withError (withLoc $here) $
        {-withProcAndSys [$here] rootPath $-}
        useEnv rootPath force $
          noisier 1 $
            withCurrentDirectory cwdPath action

-- |Set a "Revision" line in the .dsc file, and update the .changes
-- file to reflect the .dsc file's new md5sum.  By using our newdist
-- program to update the pool, this line from the .dsc file is then
-- included in the package's entry in the Sources.gz file.  Then we
-- can compare the revision from the uploaded package with the current
-- TLA revision to decide whether to build.
setRevisionInfo :: Fingerprint -> ChangesFile -> IO ChangesFile
setRevisionInfo fingerprint changes =
    case List.partition (isSuffixOf ".dsc" . changedFileName) (changeFiles changes) of
      ([file], otherFiles) -> do
            qPutStrLn ("Setting revision field in " <> changedFileName file)
            let dscFilePath = changeDir changes </> changedFileName file
            newDscFile <- parseControlFromFile dscFilePath >>= return . either (error . show) addField
            replaceFile dscFilePath (prettyShow newDscFile)
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
      (several, _) -> error ("Multiple .dsc files found in source package: " ++ intercalate ", " (map ppShow several))
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

-- forceList :: [a] -> IO [a]
-- forceList output = evaluate (length output) >> return output
