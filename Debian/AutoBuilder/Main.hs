{-# LANGUAGE FlexibleInstances, OverloadedStrings, PackageImports, ScopedTypeVariables, TemplateHaskell, TypeFamilies #-}
-- |AutoBuilder - application to build Debian packages in a clean
-- environment.  In the following list, each module's dependencies
-- appear above it:
module Debian.AutoBuilder.Main 
    ( main
    ) where

--import Control.Arrow (first)
import Control.Applicative ((<$>))
import Control.Applicative.Error (Failing(..))
import Control.Exception(SomeException, AsyncException(UserInterrupt), fromException, toException, try)
import Control.Monad(foldM, when)
import Control.Monad.Catch (MonadMask, catch, throwM)
import Control.Monad.State (MonadIO(liftIO))
import qualified Data.ByteString.Lazy as L
import Data.Either (partitionEithers)
import Data.List as List (intercalate, null, nub)
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import Data.Set as Set (Set, insert, empty, fromList, toList, null, difference)
import Data.Text (unpack)
import Data.Time(NominalDiffTime)
import Debian.AutoBuilder.BuildEnv (prepareDependOS, prepareBuildOS)
import Debian.AutoBuilder.BuildTarget (retrieve)
import qualified Debian.AutoBuilder.Params as P
import Debian.AutoBuilder.Target (buildTargets, showTargets)
import Debian.AutoBuilder.Types.Buildable (Target, targetName, Buildable(debianSourceTree), asBuildable)
import qualified Debian.AutoBuilder.Types.CacheRec as C
import Debian.AutoBuilder.Types.Download (Download(package))
import qualified Debian.AutoBuilder.Types.Packages as P
import qualified Debian.AutoBuilder.Types.ParamRec as P
import qualified Debian.AutoBuilder.Version as V
import Debian.Control (Control'(unControl), fieldValue)
import Debian.Debianize (DebT)
import Debian.Pretty (pretty)
import Debian.Release (ReleaseName(ReleaseName, relName), releaseName')
import Debian.Repo.Internal.Repos (MonadRepos, runReposCachedT, MonadReposCached)
import Debian.Repo.LocalRepository(uploadRemote, verifyUploadURI)
import Debian.Repo.MonadOS (MonadOS(getOS), evalMonadOS)
import Debian.Repo.OSImage (osLocalMaster, osLocalCopy, osBaseDistro)
import Debian.Repo.Release (Release(releaseName))
import Debian.Repo.Repo (repoReleaseInfo)
import Debian.Repo.Slice (NamedSliceList(..), SliceList(slices), Slice(sliceRepoKey),
                          appendSliceLists, inexactPathSlices, releaseSlices)
import Debian.Repo.SourceTree (control')
import Debian.Repo.State.AptImage (withAptImage)
import Debian.Repo.State.Repository (foldRepository)
import Debian.Repo.State.Slice (repoSources, updateCacheSources)
import Debian.Repo.Top (MonadTop(askTop))
import Debian.Repo.EnvPath (EnvPath(..))
import Debian.Repo.LocalRepository (LocalRepository, repoRoot)
import Debian.Repo.Prelude (runProc)
import Debian.URI(URI(uriPath, uriAuthority), URIAuth(uriUserInfo, uriRegName, uriPort), parseURI)
import Debian.Version(DebianVersion, parseDebianVersion, prettyDebianVersion)
import Distribution.Compiler (CompilerFlavor)
import Extra.Lock(withLock)
import Extra.Misc(checkSuperUser)
import Prelude hiding (null)
import System.Environment (getArgs, getEnv)
import System.Directory(createDirectoryIfMissing)
import System.Exit(ExitCode(..), exitWith)
import System.FilePath ((</>))
import qualified System.IO as IO
import System.Process (proc)
import System.Process.Progress (Output, timeTask, defaultVerbosity, withModifiedVerbosity, withModifiedVerbosity, qPutStrLn, qPutStr, ePutStrLn, ePutStr)
import System.Unix.Directory(removeRecursiveSafely)
import Text.Printf ( printf )

main :: DebT IO () -> (FilePath -> String -> P.ParamRec) -> IO ()
main init myParams =
    do IO.hPutStrLn IO.stderr "Autobuilder starting..."
       args <- getArgs
       home <- getEnv "HOME"
       -- Compute all the ParamRecs implied by the command line
       -- argument, using myParams to create each default ParamRec
       -- value.
       let recs = P.getParams args (myParams home)
       tops <- nub <$> mapM P.computeTopDir recs
       case (any P.doHelp recs, tops) of
         (False, [top]) ->
             do
                let paramSets = map (\ params -> params {P.buildPackages = P.buildTargets params (P.knownPackages params)}) recs
                results <- runReposCachedT top (foldM (doParameterSet init) [] paramSets) `catch` handle
                IO.hFlush IO.stdout
                IO.hFlush IO.stderr
                -- The result of processing a set of parameters is either an
                -- exception or a completion code.  Here we print a summary and
                -- exit with a suitable result code.
                -- ePutStrLn (intercalate "\n  " (map (\ (num, result) -> "Parameter set " ++ show num ++ ": " ++ showResult result) (zip [(1 :: Int)..] results)))
                case partitionFailing results of
                  ([], _) -> return ()
                  _ ->
                      ePutStrLn (intercalate "\n  " (map (\ (num, result) -> "Parameter set " ++ show num ++ ": " ++ showResult result) (zip [(1 :: Int)..] results))) >>
                      exitWith (ExitFailure 1)
         (True, _) -> IO.hPutStr IO.stderr (P.usage "Usage: ")
         (_, tops) -> IO.hPutStr IO.stderr ("Parameter sets have different top directories: " ++ show tops)
    where showResult (Failure ss) = intercalate "\n  " ("Failure:" : ss)
          showResult (Success _) = "Ok"
          partitionFailing :: [Failing a] -> ([[String]], [a])
          partitionFailing xs = p ([], []) xs
              where p result [] = result
                    p (fs, ss) (Failure f : more) = p (f : fs, ss) more
                    p (fs, ss) (Success s : more) = p (fs, s : ss) more
          handle (e :: SomeException) = IO.hPutStrLn IO.stderr ("Exception: " ++ show e) >> throwM e

-- |Process one set of parameters.  Usually there is only one, but there
-- can be several which are run sequentially.  Stop on first failure.
doParameterSet :: (MonadReposCached m, MonadMask m) => DebT IO () -> [Failing ([Output L.ByteString], NominalDiffTime)] -> P.ParamRec -> m [Failing ([Output L.ByteString], NominalDiffTime)]
doParameterSet init results params =
    case () of
      _ | not (Set.null badForceBuild) ->
            error $ "Invalid forceBuild target name(s): " ++ intercalate ", " (map P.unTargetName (toList badForceBuild))
        | not (Set.null badBuildTrumped) ->
            error $ "Invalid buildTrumped target name(s): " ++ intercalate ", " (map P.unTargetName (toList badBuildTrumped))
        | not (Set.null badGoals) ->
            error $ "Invalid goal target name(s): " ++ intercalate ", " (map P.unTargetName (toList badGoals))
        | not (Set.null badDiscards) ->
            error $ "Invalid discard target name(s): " ++ intercalate ", " (map P.unTargetName (toList badDiscards))
        | any isFailure results ->
            return results
      _ ->
          withModifiedVerbosity (const (P.verbosity params))
            (do top <- askTop
                withLock (top </> "lockfile") (P.buildCache params >>= runParameterSet (P.compilerFlavor params) init))
            `catch` (\ (e :: SomeException) -> return (Failure [show e])) >>=
          (\ result -> return (result : results))
    where
      badForceBuild = difference (fromList (P.forceBuild params)) allTargetNames
      badBuildTrumped = difference (fromList (P.buildTrumped params)) allTargetNames
      badGoals = difference (fromList (P.goals params)) allTargetNames
      badDiscards = difference (P.discard params) allTargetNames
      -- Set of bogus target names in the forceBuild list
      -- badTargetNames names = difference names allTargetNames
      isFailure (Failure _) = True
      isFailure _ = False
      allTargetNames :: Set P.TargetName
      allTargetNames = P.foldPackages (\ name _ _ result -> insert name result) (P.buildPackages params) empty

-- | Get the sources.list for the local upload repository associated
-- with the OSImage.  The resulting paths are for running inside the
-- build environment.
getLocalSources :: (MonadRepos m, MonadOS m, MonadIO m) => m SliceList
getLocalSources = do
  root <- repoRoot . osLocalCopy <$> getOS
  case parseURI ("file://" ++ envPath root) of
    Nothing -> error $ "Invalid local repo root: " ++ show root
    Just uri -> repoSources (Just (envRoot root)) uri

runParameterSet :: (MonadReposCached m, MonadMask m) => CompilerFlavor -> DebT IO () -> C.CacheRec -> m (Failing ([Output L.ByteString], NominalDiffTime))
runParameterSet hc init cache =
    do
      top <- askTop
      liftIO doRequiredVersion
      doVerifyBuildRepo cache
      when (P.showParams params) (withModifiedVerbosity (const defaultVerbosity) (liftIO doShowParams))
      when (P.showSources params) (withModifiedVerbosity (const defaultVerbosity) (liftIO doShowSources))
      when (P.flushAll params) (liftIO $ doFlush top)
      liftIO checkPermissions
      maybe (return ()) (verifyUploadURI (P.doSSHExport $ params)) (P.uploadURI params)
      dependOS <- prepareDependOS params buildRelease
      let allTargets = P.buildPackages params
      buildOS <- evalMonadOS (do sources <- osBaseDistro <$> getOS
                                 updateCacheSources (P.ifSourcesChanged params) sources
                                 when (P.report params) (ePutStrLn . doReport $ allTargets)
                                 qPutStr ("\n" ++ showTargets allTargets ++ "\n")
                                 getOS >>= prepareBuildOS (P.buildRelease params)) dependOS
      qPutStrLn "Retrieving all source code:\n"
      retrieved <-
          countTasks' (map (\ (target :: P.Packages) ->
                                (P.unTargetName (P.name target) <> " - " <> show (P.spec target), (Right <$> evalMonadOS (retrieve hc init cache target) buildOS) `catch` handleRetrieveException target))
                           (P.foldPackages (\ name spec flags l -> P.Package name spec flags : l) allTargets []))
      (failures, targets) <- mapM (either (return . Left)
                                          (\ download -> liftIO (try (asBuildable download)) >>=
                                                         either (\ (e :: SomeException) -> return (Left (show e)))
                                                                (\ (t :: Buildable) ->
                                                                     qPutStrLn (case unControl (control' (debianSourceTree t)) of
                                                                                  (src : bins) -> "Target " ++ P.unTargetName (P.name (package download)) ++ ", source deb: " <> maybe "(missing)" unpack (fieldValue "Source" src) <>
                                                                                        ", binary debs: [" <> intercalate ", " (map unpack (catMaybes (map (fieldValue "Package") bins))) <> "]"
                                                                                  _ -> "Invalid control file") >>
                                                                     return (Right t)))) retrieved >>=
                             return . partitionEithers
      when (not $ List.null $ failures) (error $ unlines $ "Some targets could not be retrieved:" : map ("  " ++) failures)

      -- Compute a list of sources for all the releases in the repository we will upload to,
      -- used to avoid creating package versions that already exist.  Also include the sources
      -- for the local repository to avoid collisions there as well.
      localSources <- evalMonadOS getLocalSources buildOS
      local <- evalMonadOS (osLocalMaster <$> getOS) dependOS
      let poolSources = NamedSliceList { sliceListName = ReleaseName (relName (sliceListName buildRelease) ++ "-all")
                                       , sliceList = appendSliceLists [buildRepoSources, localSources] }

      withAptImage (P.ifSourcesChanged params) poolSources
                       (buildTargets cache dependOS buildOS local targets >>=
                        upload >>=
                        liftIO . newDist)
    where
      params = C.params cache
      baseRelease =  either (error . show) id (P.findSlice cache (P.baseRelease params))
      buildRepoSources = C.buildRepoSources cache
      buildReleaseSources = releaseSlices (P.buildRelease params) (inexactPathSlices buildRepoSources)
      buildRelease = NamedSliceList { sliceListName = ReleaseName (releaseName' (P.buildRelease params))
                                    , sliceList = appendSliceLists [sliceList baseRelease, buildReleaseSources] }
      doRequiredVersion :: IO ()
      doRequiredVersion =
          let abv = parseDebianVersion V.autoBuilderVersion
              rqvs = P.requiredVersion params in
          case filter (\ (v, _) -> v > abv) rqvs of
            [] -> return ()
            reasons ->
                do ePutStrLn ("Installed autobuilder library version " ++ V.autoBuilderVersion ++ " is too old:")
                   mapM_ printReason reasons
                   liftIO $ exitWith (ExitFailure 1)
          where
            printReason :: (DebianVersion, Maybe String) -> IO ()
            printReason (v, s) =
                ePutStr (" Version >= " ++ show (prettyDebianVersion v) ++ " is required" ++ maybe "" ((++) ":") s)
      doShowParams = ePutStr $ "Configuration parameters:\n" ++ P.prettyPrint params
      doShowSources =
          either (error . show) doShow (P.findSlice cache (ReleaseName (releaseName' (P.buildRelease params))))
          where
            doShow sources =
                do qPutStrLn $ (relName . sliceListName $ sources) ++ ":"
                   qPutStrLn . show . pretty . sliceList $ sources
                   exitWith ExitSuccess
      doFlush top =
          do qPutStrLn "Flushing cache"
             removeRecursiveSafely top
             createDirectoryIfMissing True top
      checkPermissions =
          do isRoot <- liftIO $ checkSuperUser
             case isRoot of
               True -> return ()
               False -> do qPutStr "You must be superuser to run the autobuilder (to use chroot environments.)"
                           liftIO $ exitWith (ExitFailure 1)
      upload :: MonadRepos m => (LocalRepository, [Target]) -> m [Failing ([Output L.ByteString], NominalDiffTime)]
      upload (repo, [])
          | P.doUpload params =
              case P.uploadURI params of
                Nothing -> error "Cannot upload, no 'Upload-URI' parameter given"
                Just uri -> qPutStrLn "Uploading from local repository to remote" >> liftIO (uploadRemote repo uri)
          | True = return []
      upload (_, failed) =
          do let msg = ("Some targets failed to build:\n  " ++ intercalate "\n  " (map (P.unTargetName . targetName) failed))
             qPutStrLn msg
             case P.doUpload params of
               True -> qPutStrLn "Skipping upload."
               False -> return ()
             error msg
      newDist :: [Failing ([Output L.ByteString], NominalDiffTime)] -> IO (Failing ([Output L.ByteString], NominalDiffTime))
      newDist _results
          | P.doNewDist params =
              case P.uploadURI params of
                Just uri ->
                    case uriAuthority uri of
                         Just auth ->
                             let cmd = "ssh"
                                 args = [uriUserInfo auth ++ uriRegName auth, P.newDistProgram params,
                                         "--sign", "--root", uriPath uri] ++
                                        (concat . map (\ rel -> ["--create", rel]) . P.createRelease $ params) in
                             qPutStrLn "Running newdist on remote repository" >>
                             try (timeTask (runProc (proc cmd args))) >>= return . either (\ (e :: SomeException) -> Failure [show e]) Success
                         Nothing ->
                             let cmd = P.newDistProgram params
                                 args = ["--sign", "--root", uriPath uri] in
                             qPutStr "Running newdist on a local repository" >>
                             try (timeTask (runProc (proc cmd args))) >>= return . either (\ (e :: SomeException) -> Failure [show e]) Success
                _ -> error "Missing Upload-URI parameter"
          | True = return (Success ([], (fromInteger 0)))

-- | Make sure the build release ("P.buildRelease params") - the
-- release and repository to which we intend to upload the packages
-- that we build - exists on the upload server ("P.uploadURI params").
doVerifyBuildRepo :: MonadRepos m => C.CacheRec -> m ()
doVerifyBuildRepo cache =
    do repoNames <- mapM (foldRepository f g) (map sliceRepoKey . slices . C.buildRepoSources $ cache) >>= return . map releaseName . concat
       when (not (any (== (P.buildRelease params)) repoNames))
            (case P.uploadURI params of
               Nothing -> error "No uploadURI?"
               Just uri ->
                   let ssh = case uriAuthority uri of
                               Just auth -> uriUserInfo auth ++ uriRegName auth ++ uriPort auth
                               Nothing -> "user@hostname"
                       rel = releaseName' (P.buildRelease params)
                       top = uriPath uri in -- "/home/autobuilder/deb-private/debian"
                   error $ "Build repository does not exist on remote server: " ++ rel ++ "\nUse newdist there to create it:" ++
                           "\n  ssh " ++ ssh ++ " " ++ P.newDistProgram params ++ " --root=" ++ top ++ " --create-release=" ++ rel ++
                           "\n  ssh " ++ ssh ++ " " ++ P.newDistProgram params ++ " --root=" ++ top ++ " --create-section=" ++ rel ++ ",main" ++
                           "\nYou will also need to remove the local file ~/.autobuilder/repoCache.")
    where
      f = return . repoReleaseInfo
      g = return . repoReleaseInfo
      params = C.params cache

handleRetrieveException :: MonadReposCached m => P.Packages -> SomeException -> m (Either String Download)
handleRetrieveException target e =
          case (fromException (toException e) :: Maybe AsyncException) of
            Just UserInterrupt ->
                throwM e -- break out of loop
            _ -> let message = ("Failure retrieving " ++ show (P.spec target) ++ ":\n  " ++ show e) in
                 liftIO (IO.hPutStrLn IO.stderr message) >> return (Left message)

-- | Perform a list of tasks with log messages.
countTasks' :: MonadIO m => [(String, m a)] -> m [a]
countTasks' tasks =
    mapM (countTask (length tasks)) (zip [1..] tasks)
    where
      countTask :: MonadIO m => Int -> (Int, (String, m a)) -> m a
      countTask count (index, (message, task)) =
          liftIO (IO.hPutStrLn IO.stderr (printf "[%2d of %2d] %s:" index count (limit 100 message))) >>
          task >>= \ a ->
          return a

limit n s = if length s > n + 3 then take n s ++ "..." else s

doReport :: P.Packages -> String
doReport =
    intercalate "\n" . doReport'
    where
      doReport' :: P.Packages -> [String]
      doReport' P.NoPackage = []
      doReport' p@(P.Packages {}) = concatMap doReport' (P.list p)
      doReport' p@(P.Package {}) =
          patched (P.spec p) ++ pinned (P.flags p)
          where
            patched :: P.RetrieveMethod -> [String]
            patched (P.Patch _ _) = [P.unTargetName (P.name p) ++ " is patched"]
            patched (P.Cd _ x) = patched x
            patched (P.DataFiles x y _) = patched x ++ patched y
            patched (P.DebDir x y) = patched x ++ patched y
            patched (P.Debianize x) = patched x
            patched (P.Proc x) = patched x
            patched (P.Quilt x y) = patched x ++ patched y
            patched (P.SourceDeb x) = patched x
            patched (P.Twice x) = patched x
            patched _ = []
            pinned :: [P.PackageFlag] -> [String]
            pinned [] = []
            pinned (P.CabalPin v : more) = [P.unTargetName (P.name p) ++ " is pinned at version " ++ v] ++ pinned more
            pinned (_ : more) = pinned more
