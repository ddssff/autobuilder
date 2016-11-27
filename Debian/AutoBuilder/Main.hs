{-# LANGUAGE CPP, FlexibleContexts, FlexibleInstances, OverloadedStrings, PackageImports, ScopedTypeVariables, TemplateHaskell, TypeFamilies #-}
-- |AutoBuilder - application to build Debian packages in a clean
-- environment.  In the following list, each module's dependencies
-- appear above it:
module Debian.AutoBuilder.Main
    ( main
    ) where

import Control.Arrow (first)
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative (Applicative, (<$>))
import Data.Monoid (mconcat, mempty)
#endif
import Control.Applicative.Error (Failing(..), ErrorMsg)
import Control.Exception(SomeException, AsyncException(UserInterrupt), fromException, toException)
import Control.Lens (view)
import Control.Monad(foldM, when)
import Control.Monad.Catch (MonadMask, catch, throwM)
import Control.Monad.State (MonadIO(liftIO))
import qualified Data.ByteString.Lazy as L
import Data.Either (partitionEithers)
import Data.Generics (listify)
import Data.List as List (intercalate, null, nub)
import Data.Map ((!))
import Data.Monoid ((<>))
import Data.Set as Set (toList)
import Debian.AutoBuilder.BuildEnv (prepareDependOS, prepareBuildOS)
import Debian.AutoBuilder.BuildTarget (retrieve)
import qualified Debian.AutoBuilder.Params as P (computeTopDir, buildCache, baseRelease, findSlice)
import Debian.AutoBuilder.Target (buildTargets, showTargets)
import Debian.AutoBuilder.Types.Buildable (Target, Buildable(debianSourceTree), asBuildable)
import Debian.AutoBuilder.Types.Download (SomeDownload(..), Download(..))
import qualified Debian.AutoBuilder.Types.CacheRec as C
import qualified Debian.AutoBuilder.Types.Packages as P (spec, flags, post, PackageFlag(CabalPin))
import qualified Debian.AutoBuilder.Types.ParamRec as R
import qualified Debian.AutoBuilder.Version as V
import Debian.Control.Policy (debianPackageNames, debianSourcePackageName)
import Debian.Debianize (CabalT, CabalInfo)
import Debian.GHC (hvrCompilerPATH, withModifiedPATH)
import Debian.Pretty (prettyShow, ppShow)
import Debian.Relation (BinPkgName(unBinPkgName), SrcPkgName(unSrcPkgName))
import Debian.Release (ReleaseName(ReleaseName, relName), releaseName')
import Debian.Repo.EnvPath (EnvRoot(rootPath))
import qualified Debian.Repo.Fingerprint as P (RetrieveMethod(Patch, Cd, DebDir, DataFiles, Debianize'', Proc, Quilt, SourceDeb, Twice, Zero))
import Debian.Repo.Internal.Repos (MonadRepos, runReposCachedT, MonadReposCached)
import Debian.Repo.LocalRepository(uploadRemote, verifyUploadURI)
import Debian.Repo.MonadOS (MonadOS(getOS), evalMonadOS)
import Debian.Repo.OSImage (osLocalMaster, osLocalCopy, osBaseDistro)
import Debian.Repo.Prelude.Process (readProcessVE)
import Debian.Repo.Prelude.Verbosity (ePutStrLn, ePutStr, qPutStrLn, qPutStr, withVerbosity, noisier)
import Debian.Repo.Release (Release(releaseName))
import Debian.Repo.Repo (repoReleaseInfo)
import Debian.Repo.Slice (NamedSliceList(..), SliceList(slices), Slice(sliceRepoKey),
                          appendSliceLists, inexactPathSlices, releaseSlices, expandPPASlice)
import Debian.Repo.State.AptImage (withAptImage)
import Debian.Repo.State.Repository (foldRepository)
import Debian.Repo.State.Slice (repoSources, updateCacheSources)
import Debian.Repo.Top (MonadTop(askTop))
import Debian.Repo.EnvPath (EnvPath(..))
import Debian.Repo.LocalRepository (LocalRepository, repoRoot)
import Debian.URI(URI(uriPath, uriAuthority), URIAuth(uriUserInfo, uriRegName, uriPort), parseURI)
import Debian.Version(DebianVersion, parseDebianVersion', prettyDebianVersion)
import Extra.Lock(withLock)
import Extra.Misc(checkSuperUser)
import Prelude hiding (null)
import System.Environment (getArgs, getEnv)
import System.Directory(createDirectoryIfMissing)
import System.Exit(ExitCode(..), exitWith)
import System.FilePath ((</>))
import System.IO as IO
import System.Process (proc, cmdspec)
import System.Process.ListLike (showCmdSpecForUser)
-- import System.Process.Read.Verbosity (defaultVerbosity, withModifiedVerbosity, withModifiedVerbosity)
import System.Unix.Directory(removeRecursiveSafely)
import System.Unix.Mount (withProcAndSys)
import Text.Printf ( printf )

main :: CabalT IO () -> (FilePath -> String -> R.ParamRec) -> IO ()
main init myParams =
    do IO.hPutStrLn IO.stderr "Autobuilder starting..."
       args <- getArgs
       home <- getEnv "HOME"
       -- Compute all the ParamRecs implied by the command line
       -- argument, using myParams to create each default ParamRec
       -- value.
       let recs = R.getParams args (myParams home)
       when (any R.doHelp recs) (IO.hPutStr IO.stderr (R.usage "Usage: ") >> exitWith ExitSuccess)
       tops <- nub <$> mapM P.computeTopDir recs
       case tops of
         -- All the work for a given run must occur in the same top
         -- directory - ~/.autobuilder for example.
         [top] -> runReposCachedT top (foldM (doParameterSet init) [] recs) `catch` showAndThrow >>= testResults
         [] -> IO.hPutStrLn IO.stderr "No parameter sets"
         tops -> IO.hPutStrLn IO.stderr ("Parameter sets have different top directories: " ++ show tops)
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
doParameterSet :: (Applicative m, MonadReposCached m, MonadMask m) => CabalT IO () -> [Failing (ExitCode, L.ByteString, L.ByteString)] -> R.ParamRec -> m [Failing (ExitCode, L.ByteString, L.ByteString)]
-- If one parameter set fails, don't try the rest.  Not sure if
-- this is the right thing, but it is safe.
doParameterSet _ results _ | any isFailure results = return results
doParameterSet init results params = do
  result <- withVerbosity (R.verbosity params)
            (do top <- askTop
                withLock (top </> "lockfile") $
                  -- Should we just let the autobuilder deduce from current $PATH?
                  -- Probably not, because packages built with ghc are difficult to
                  -- distinguish from those built with ghc-8.0.1 as things stand.  But then
                  -- we need our --hvr-version option back.
                  withModifiedPATH (maybe id hvrCompilerPATH (R.hvrVersion params)) $
                    P.buildCache params >>= runParameterSet init)
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

runParameterSet :: (Applicative m, MonadReposCached m, MonadMask m) => CabalT IO () -> C.CacheRec -> m (Failing (ExitCode, L.ByteString, L.ByteString))
runParameterSet init cache =
    do
      top <- askTop
      liftIO doRequiredVersion
      doVerifyBuildRepo cache
      when (R.showParams params) (withVerbosity 1 (liftIO doShowParams))
      when (R.showSources params) (withVerbosity 1 (liftIO doShowSources))
      when (R.flushAll params) (liftIO $ doFlush top)
      liftIO checkPermissions
      maybe (return ()) (verifyUploadURI (R.doSSHExport $ params)) (R.uploadURI params)
      qPutStrLn "Preparing dependency environment"
      extraSlices <- mapM (either (return . (: [])) (liftIO . expandPPASlice (P.baseRelease params))) (R.extraRepos params) >>= return . concat
      dependOS <- prepareDependOS params buildRelease extraSlices
      let allTargets :: [(P.RetrieveMethod, [P.PackageFlag], [CabalInfo -> CabalInfo])]
          allTargets = map (\i -> f (R.knownPackages (C.params cache) ! i)) (Set.toList (R.buildPackages params))
          f p = (view P.spec p, view P.flags p, view P.post p)
      -- let allTargets = filter (notZero . view _1) (P.foldPackages (\ p l -> (view P.spec p, view P.flags p, view P.post p) : l) (R.buildPackages params) [])
      qPutStrLn "Preparing build environment"
      buildOS <- evalMonadOS (do sources <- osBaseDistro <$> getOS
                                 updateCacheSources (R.ifSourcesChanged params) sources
                                 when (R.report params) (ePutStrLn . doReport $ allTargets)
                                 qPutStr ("\n" ++ showTargets allTargets ++ "\n")
                                 getOS >>= prepareBuildOS (R.buildRelease params)) dependOS
      qPutStrLn "Retrieving all source code:\n"
      (failures, targets) <- partitionEithers <$> (mapM (uncurry (retrieveTarget dependOS (length allTargets))) (zip [1..] allTargets))
      when (not $ List.null $ failures) (error $ unlines $ "Some targets could not be retrieved:" : map ("  " ++) failures)

      -- Compute a list of sources for all the releases in the repository we will upload to,
      -- used to avoid creating package versions that already exist.  Also include the sources
      -- for the local repository to avoid collisions there as well.
      localSources <- evalMonadOS getLocalSources buildOS
      local <- evalMonadOS (osLocalMaster <$> getOS) dependOS
      let poolSources = NamedSliceList { sliceListName = ReleaseName (relName (sliceListName buildRelease) ++ "-all")
                                       , sliceList = appendSliceLists [buildRepoSources, localSources] }

      withAptImage (R.ifSourcesChanged params) poolSources $ do
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
      retrieveTarget :: (MonadReposCached m) => EnvRoot -> Int -> Int -> (P.RetrieveMethod, [P.PackageFlag], [CabalInfo -> CabalInfo]) -> m (Either String (Buildable SomeDownload))
      retrieveTarget dependOS count index (method, flags, functions) = do
            liftIO (hPutStr stderr (printf "[%2d of %2d]" index count))
            res <- (Right <$> evalMonadOS (do download <- withProcAndSys (rootPath dependOS) $ retrieve init cache method flags functions
                                              when (R.flushSource params) (flushSource download)
                                              buildable <- liftIO (asBuildable download)
                                              let (src, bins) = debianPackageNames (debianSourceTree buildable)
                                              liftIO (hPutStrLn stderr (printf " %s - %s:" (unSrcPkgName src) (limit 100 (show method) :: String)))
                                              qPutStrLn $ "Binary debs: [" <> intercalate ", " (map unBinPkgName bins) <> "]"
                                              return buildable) dependOS) `catch` handleRetrieveException method
            return res
      params = C.params cache
      baseRelease =  either (error . show) id (P.findSlice cache (P.baseRelease params))
      buildRepoSources = C.buildRepoSources cache
      buildReleaseSources = releaseSlices (R.buildRelease params) (inexactPathSlices buildRepoSources)
      buildRelease = NamedSliceList { sliceListName = ReleaseName (releaseName' (R.buildRelease params))
                                    , sliceList = appendSliceLists [sliceList baseRelease, buildReleaseSources] }
      doRequiredVersion :: IO ()
      doRequiredVersion =
          let abv = parseDebianVersion' V.autoBuilderVersion
              rqvs = R.requiredVersion params in
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
      doShowParams = ePutStr $ "Configuration parameters:\n" ++ R.prettyPrint params
      doShowSources =
          either (error . show) doShow (P.findSlice cache (ReleaseName (releaseName' (R.buildRelease params))))
          where
            doShow sources =
                do qPutStrLn $ (relName . sliceListName $ sources) ++ ":"
                   qPutStrLn . prettyShow . sliceList $ sources
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
          | R.doUpload params =
              case R.uploadURI params of
                Nothing -> error "Cannot upload, no 'Upload-URI' parameter given"
                Just uri -> qPutStrLn "Uploading from local repository to remote" >> liftIO (uploadRemote repo uri)
          | True = return []
      upload (_, failed) =
          do let msg = ("Some targets failed to build:\n  " ++ intercalate "\n  " (map (ppShow . debianSourcePackageName) failed))
             qPutStrLn msg
             case R.doUpload params of
               True -> qPutStrLn "Skipping upload."
               False -> return ()
             error msg
      newDist :: ([ErrorMsg], [(ExitCode, L.ByteString, L.ByteString)]) -> IO (Failing (ExitCode, L.ByteString, L.ByteString))
      newDist ([], _)
          | R.doNewDist params =
              case R.uploadURI params of
                Just uri ->
                    do let p = case uriAuthority uri of
                                 Just auth ->
                                     let cmd = "ssh"
                                         args = [uriUserInfo auth ++ uriRegName auth, R.newDistProgram params,
                                                 "--sign", "--root", uriPath uri] ++
                                                (concat . map (\ rel -> ["--create", rel]) . R.createRelease $ params) in
                                     (proc cmd args)
                                 _ ->
                                     let cmd = R.newDistProgram params
                                         args = ["--sign", "--root", uriPath uri] in
                                     (proc cmd args)
                       qPutStrLn (" -> " ++ showCmdSpecForUser (cmdspec p))
                       result <- readProcessVE p L.empty >>= return . either (\ (e :: SomeException) -> Failure [show e]) testOutput
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
       when (not (any (== (R.buildRelease params)) repoNames))
            (case R.uploadURI params of
               Nothing -> error "No uploadURI?"
               Just uri ->
                   let ssh = case uriAuthority uri of
                               Just auth -> uriUserInfo auth ++ uriRegName auth ++ uriPort auth
                               Nothing -> "user@hostname"
                       rel = releaseName' (R.buildRelease params)
                       top = uriPath uri in -- "/home/autobuilder/deb-private/debian"
                   error $ "Build repository does not exist on remote server: " ++ rel ++ "\nUse newdist there to create it:" ++
                           "\n  ssh " ++ ssh ++ " " ++ R.newDistProgram params ++ " --root=" ++ top ++ " --create-release=" ++ rel ++
                           "\n  ssh " ++ ssh ++ " " ++ R.newDistProgram params ++ " --root=" ++ top ++ " --create-section=" ++ rel ++ ",main" ++
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

doReport :: [(P.RetrieveMethod, [P.PackageFlag], [CabalInfo -> CabalInfo])] -> String
doReport =
    intercalate "\n" . concatMap doReport'
    where
      doReport' :: (P.RetrieveMethod, [P.PackageFlag], [CabalInfo -> CabalInfo]) -> [String]
      doReport' (spec, flags, fns) =
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
