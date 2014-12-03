{-# LANGUAGE CPP, FlexibleInstances, OverloadedStrings, PackageImports, ScopedTypeVariables, TemplateHaskell, TypeFamilies #-}
-- |AutoBuilder - application to build Debian packages in a clean
-- environment.  In the following list, each module's dependencies
-- appear above it:
module Debian.AutoBuilder.Main 
    ( main
    ) where

import Control.Arrow (first)
import Control.Applicative (Applicative, (<$>))
import Control.Applicative.Error (Failing(..), ErrorMsg)
import Control.Exception(SomeException, AsyncException(UserInterrupt), fromException, toException, try)
import Control.Monad(foldM, when)
import Control.Monad.Catch (MonadMask, catch, throwM)
import Control.Monad.State (MonadIO(liftIO))
import qualified Data.ByteString.Lazy as L
import Data.Either (partitionEithers)
import Data.Generics (listify)
import Data.List as List (intercalate, null, nub)
import Data.Monoid ((<>), mconcat, mempty)
import Data.Text as LT (unpack)
import Data.Text.Encoding (decodeUtf8)
import Data.Time(NominalDiffTime)
import Debian.AutoBuilder.BuildEnv (prepareDependOS, prepareBuildOS)
import Debian.AutoBuilder.BuildTarget (retrieve)
import qualified Debian.AutoBuilder.Params as P (computeTopDir, buildCache, baseRelease, findSlice)
import Debian.AutoBuilder.Target (buildTargets, showTargets)
import Debian.AutoBuilder.Types.Buildable (Target, Buildable(debianSourceTree), asBuildable)
import Debian.AutoBuilder.Types.Download (SomeDownload(..), Download(..))
import qualified Debian.AutoBuilder.Types.CacheRec as C
import qualified Debian.AutoBuilder.Types.Packages as P (foldPackages, spec, flags, PackageFlag(CabalPin))
import qualified Debian.AutoBuilder.Types.ParamRec as P (ParamRec, getParams, doHelp, usage, verbosity, showParams, showSources, flushAll, doSSHExport, uploadURI, report, buildRelease, ifSourcesChanged, requiredVersion, prettyPrint, doUpload, doNewDist, newDistProgram, createRelease, buildPackages, flushSource)
import qualified Debian.AutoBuilder.Version as V
import Debian.Control.Policy (debianPackageNames, debianSourcePackageName)
import Debian.Debianize (DebT)
import Debian.Pretty (ppDisplay)
import Debian.Relation (BinPkgName(unBinPkgName), SrcPkgName(unSrcPkgName))
import Debian.Release (ReleaseName(ReleaseName, relName), releaseName')
import Debian.Repo.EnvPath (EnvRoot)
import qualified Debian.Repo.Fingerprint as P (RetrieveMethod(Patch, Cd, DebDir, DataFiles, Debianize'', Proc, Quilt, SourceDeb, Twice, Zero))
import Debian.Repo.Internal.Repos (MonadRepos, runReposCachedT, MonadReposCached)
import Debian.Repo.LocalRepository(uploadRemote, verifyUploadURI)
import Debian.Repo.MonadOS (MonadOS(getOS), evalMonadOS)
import Debian.Repo.OSImage (osLocalMaster, osLocalCopy, osBaseDistro)
import Debian.Repo.Prelude.Process (readProcessE)
import Debian.Repo.Prelude.Verbosity (ePutStrLn, ePutStr, qPutStrLn, qPutStr, withVerbosity, noisier)
import Debian.Repo.Release (Release(releaseName))
import Debian.Repo.Repo (repoReleaseInfo)
import Debian.Repo.Slice (NamedSliceList(..), SliceList(slices), Slice(sliceRepoKey),
                          appendSliceLists, inexactPathSlices, releaseSlices)
import Debian.Repo.State.AptImage (withAptImage)
import Debian.Repo.State.Repository (foldRepository)
import Debian.Repo.State.Slice (repoSources, updateCacheSources)
import Debian.Repo.Top (MonadTop(askTop))
import Debian.Repo.EnvPath (EnvPath(..))
import Debian.Repo.LocalRepository (LocalRepository, repoRoot)
import Debian.URI(URI(uriPath, uriAuthority), URIAuth(uriUserInfo, uriRegName, uriPort), parseURI)
import Debian.Version(DebianVersion, parseDebianVersion, prettyDebianVersion)
import Extra.Lock(withLock)
import Extra.Misc(checkSuperUser)
import Prelude hiding (null)
import System.Environment (getArgs, getEnv)
import System.Directory(createDirectoryIfMissing)
import System.Exit(ExitCode(..), exitWith)
import System.FilePath ((</>))
import System.IO as IO
import System.Process (proc, cmdspec)
import System.Process.Extras (showCmdSpecForUser)
-- import System.Process.Read.Verbosity (defaultVerbosity, withModifiedVerbosity, withModifiedVerbosity)
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
       when (any P.doHelp recs) (IO.hPutStr IO.stderr (P.usage "Usage: ") >> exitWith ExitSuccess)
       tops <- nub <$> mapM P.computeTopDir recs
       case tops of
         -- All the work for a given run must occur in the same top
         -- directory - ~/.autobuilder for example.
         [top] -> runReposCachedT top (foldM (doParameterSet init) [] recs) `catch` showAndThrow >>= testResults
         tops -> IO.hPutStr IO.stderr ("Parameter sets have different top directories: " ++ show tops)
    where testResults results =
              case partitionFailing results of
                ([], _) -> return ()
                _ ->
                    ePutStrLn (intercalate "\n  " (map (uncurry showResult) (zip [(1 :: Int)..] results))) >>
                    exitWith (ExitFailure 1)
          showResult num result =
              "Parameter set " ++ show num ++ ": " ++  case result of
                                                         Failure ss -> intercalate "\n  " ("Failure:" : ss)
                                                         Success _ -> "Ok"
          showAndThrow (e :: SomeException) = IO.hPutStrLn IO.stderr ("Exception: " ++ show e) >> throwM e

partitionFailing :: [Failing a] -> ([[String]], [a])
partitionFailing xs = p ([], []) xs
    where p result [] = result
          p (fs, ss) (Failure f : more) = p (f : fs, ss) more
          p (fs, ss) (Success s : more) = p (fs, s : ss) more

isFailure :: Failing a -> Bool
isFailure (Failure _) = True
isFailure _ = False

-- |Process one set of parameters.  Usually there is only one, but there
-- can be several which are run sequentially.  Stop on first failure.
doParameterSet :: (Applicative m, MonadReposCached m, MonadMask m) => DebT IO () -> [Failing (ExitCode, L.ByteString, L.ByteString)] -> P.ParamRec -> m [Failing (ExitCode, L.ByteString, L.ByteString)]
-- If one parameter set fails, don't try the rest.  Not sure if
-- this is the right thing, but it is safe.
doParameterSet _ results _ | any isFailure results = return results
doParameterSet init results params = do
  result <- withVerbosity (P.verbosity params)
            (do top <- askTop
                withLock (top </> "lockfile") (P.buildCache params >>= runParameterSet init))
              `catch` (\ (e :: SomeException) -> return (Failure [show e]))
  return (result : results)

-- | Get the sources.list for the local upload repository associated
-- with the OSImage.  The resulting paths are for running inside the
-- build environment.
getLocalSources :: (MonadRepos m, MonadOS m, MonadIO m) => m SliceList
getLocalSources = do
  root <- repoRoot . osLocalCopy <$> getOS
  case parseURI ("file://" ++ envPath root) of
    Nothing -> error $ "Invalid local repo root: " ++ show root
    Just uri -> repoSources (Just (envRoot root)) uri

runParameterSet :: (Applicative m, MonadReposCached m, MonadMask m) => DebT IO () -> C.CacheRec -> m (Failing (ExitCode, L.ByteString, L.ByteString))
runParameterSet init cache =
    do
      top <- askTop
      liftIO doRequiredVersion
      doVerifyBuildRepo cache
      when (P.showParams params) (withVerbosity 1 (liftIO doShowParams))
      when (P.showSources params) (withVerbosity 1 (liftIO doShowSources))
      when (P.flushAll params) (liftIO $ doFlush top)
      liftIO checkPermissions
      maybe (return ()) (verifyUploadURI (P.doSSHExport $ params)) (P.uploadURI params)
      qPutStrLn "Preparing dependency environment"
      dependOS <- prepareDependOS params buildRelease
      let allTargets = filter (notZero . fst) (P.foldPackages (\ p l -> (P.spec p, P.flags p) : l) (P.buildPackages params) [])
      qPutStrLn "Preparing build environment"
      buildOS <- evalMonadOS (do sources <- osBaseDistro <$> getOS
                                 updateCacheSources (P.ifSourcesChanged params) sources
                                 when (P.report params) (ePutStrLn . doReport $ allTargets)
                                 qPutStr ("\n" ++ showTargets allTargets ++ "\n")
                                 getOS >>= prepareBuildOS (P.buildRelease params)) dependOS
      qPutStrLn "Retrieving all source code:\n"
      (failures, targets) <- partitionEithers <$> (mapM (uncurry (retrieveTarget buildOS (length allTargets))) (zip [1..] allTargets))
      when (not $ List.null $ failures) (error $ unlines $ "Some targets could not be retrieved:" : map ("  " ++) failures)

      -- Compute a list of sources for all the releases in the repository we will upload to,
      -- used to avoid creating package versions that already exist.  Also include the sources
      -- for the local repository to avoid collisions there as well.
      localSources <- evalMonadOS getLocalSources buildOS
      local <- evalMonadOS (osLocalMaster <$> getOS) dependOS
      let poolSources = NamedSliceList { sliceListName = ReleaseName (relName (sliceListName buildRelease) ++ "-all")
                                       , sliceList = appendSliceLists [buildRepoSources, localSources] }

      withAptImage (P.ifSourcesChanged params) poolSources $ do
        buildResult <- buildTargets cache dependOS buildOS local targets
        uploadResult <- noisier 1 $ upload buildResult
        liftIO $ newDist (partitionFailing uploadResult)
    where
      partitionFailing :: [Failing a] -> ([ErrorMsg], [a])
      partitionFailing xs =
          first concat $
          partitionEithers $
          map (\ x -> case x of
                        (Failure ms) -> Left ms
                        (Success a) -> Right a) xs
      notZero x = null (listify (\ x -> case x of P.Zero -> True; _ -> False) x)
      retrieveTarget :: (MonadReposCached m) => EnvRoot -> Int -> Int -> (P.RetrieveMethod, [P.PackageFlag]) -> m (Either String (Buildable SomeDownload))
      retrieveTarget buildOS count index (method, flags) = do
            liftIO (hPutStr stderr (printf "[%2d of %2d]" index count))
            res <- (Right <$> evalMonadOS (do download <- retrieve init cache method flags
                                              when (P.flushSource params) (flushSource download)
                                              buildable <- liftIO (asBuildable download)
                                              let (src, bins) = debianPackageNames (debianSourceTree buildable)
                                              liftIO (hPutStrLn stderr (printf " %s - %s:" (unSrcPkgName src) (limit 100 (show method) :: String)))
                                              qPutStrLn $ "Binary debs: [" <> intercalate ", " (map unBinPkgName bins) <> "]"
                                              return buildable) buildOS) `catch` handleRetrieveException method
            return res
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
                   qPutStrLn . ppDisplay . sliceList $ sources
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
      upload :: MonadRepos m => (LocalRepository, [Target SomeDownload]) -> m [Failing (ExitCode, L.ByteString, L.ByteString)]
      upload (repo, [])
          | P.doUpload params =
              case P.uploadURI params of
                Nothing -> error "Cannot upload, no 'Upload-URI' parameter given"
                Just uri -> qPutStrLn "Uploading from local repository to remote" >> liftIO (uploadRemote repo uri)
          | True = return []
      upload (_, failed) =
          do let msg = ("Some targets failed to build:\n  " ++ intercalate "\n  " (map (ppDisplay . debianSourcePackageName) failed))
             qPutStrLn msg
             case P.doUpload params of
               True -> qPutStrLn "Skipping upload."
               False -> return ()
             error msg
      newDist :: ([ErrorMsg], [(ExitCode, L.ByteString, L.ByteString)]) -> IO (Failing (ExitCode, L.ByteString, L.ByteString))
      newDist ([], _)
          | P.doNewDist params =
              case P.uploadURI params of
                Just uri ->
                    do let p = case uriAuthority uri of
                                 Just auth ->
                                     let cmd = "ssh"
                                         args = [uriUserInfo auth ++ uriRegName auth, P.newDistProgram params,
                                                 "--sign", "--root", uriPath uri] ++
                                                (concat . map (\ rel -> ["--create", rel]) . P.createRelease $ params) in
                                     (proc cmd args)
                                 _ ->
                                     let cmd = P.newDistProgram params
                                         args = ["--sign", "--root", uriPath uri] in
                                     (proc cmd args)
                       qPutStrLn (" -> " ++ showCmdSpecForUser (cmdspec p))
                       result <- readProcessE p L.empty >>= return . either (\ (e :: SomeException) -> Failure [show e]) testOutput
                       case result of
                         (Success _) -> return result
                         (Failure msgs) -> ePutStrLn (intercalate "\n " ("newdist failed:" : msgs)) >> return result
                       return result
                _ -> error "Missing Upload-URI parameter"
          | True = return (Success mempty)
      newDist (errors, _) = return $ Failure errors

      testOutput :: (ExitCode, L.ByteString, L.ByteString) -> Failing (ExitCode, L.ByteString, L.ByteString)
      testOutput result@(ExitSuccess, _, _) = Success result
      testOutput (code, out, err) =
          Failure [show code <> "\n" <> ({-LT.unpack $ decodeUtf8 $ mconcat $ L.toChunks $ snd $ indentChunks " 1> " " 2> "-} show (out, err))]

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
                           "\nYou will also need to remove the local file ~/.autobuilder/repoCache." ++
                           "\n(Available: " ++ show repoNames ++ ")")
    where
      f = return . repoReleaseInfo
      g = return . repoReleaseInfo
      params = C.params cache

handleRetrieveException :: MonadReposCached m => P.RetrieveMethod -> SomeException -> m (Either String (Buildable SomeDownload))
handleRetrieveException method e =
          case (fromException (toException e) :: Maybe AsyncException) of
            Just UserInterrupt ->
                throwM e -- break out of loop
            _ -> let message = ("Failure retrieving " ++ show method ++ ":\n  " ++ show e) in
                 liftIO (IO.hPutStrLn IO.stderr message) >> return (Left message)

limit n s = if length s > n + 3 then take n s ++ "..." else s

doReport :: [(P.RetrieveMethod, [P.PackageFlag])] -> String
doReport =
    intercalate "\n" . concatMap doReport'
    where
      doReport' :: (P.RetrieveMethod, [P.PackageFlag]) -> [String]
      doReport' (spec, flags) =
          patched spec ++ pinned flags
          where
            patched :: P.RetrieveMethod -> [String]
            patched (P.Patch _ _) = [show spec ++ " is patched"]
            patched (P.Cd _ x) = patched x
            patched (P.DataFiles x y _) = patched x ++ patched y
            patched (P.DebDir x y) = patched x ++ patched y
            patched (P.Debianize'' x _) = patched x
            patched (P.Proc x) = patched x
            patched (P.Quilt x y) = patched x ++ patched y
            patched (P.SourceDeb x) = patched x
            patched (P.Twice x) = patched x
            patched _ = []
            pinned :: [P.PackageFlag] -> [String]
            pinned [] = []
            pinned (P.CabalPin v : more) = [show spec ++ " is pinned at version " ++ v] ++ pinned more
            pinned (_ : more) = pinned more
