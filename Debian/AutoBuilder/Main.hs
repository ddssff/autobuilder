{-# LANGUAGE CPP, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, PackageImports,
             RankNTypes, ScopedTypeVariables, TemplateHaskell, TypeFamilies #-}
-- |AutoBuilder - application to build Debian packages in a clean
-- environment.  In the following list, each module's dependencies
-- appear above it:
module Debian.AutoBuilder.Main
    ( main
    ) where

import Control.Applicative.Error (Failing(..))
import Control.Exception (AsyncException(UserInterrupt), Exception, fromException, SomeException, toException)
import Control.Lens (to, view)
import Control.Monad(when)
import Control.Monad.Catch (catch, MonadCatch, MonadMask, MonadThrow, throwM)
import Control.Monad.Reader (runReaderT)
import Control.Monad.State (MonadIO(liftIO), MonadState)
import qualified Data.ByteString.Lazy as L
import Data.Either (partitionEithers)
--import Data.Generics (listify)
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
import Debian.Codename (codename, parseCodename)
import Debian.Control.Policy (debianPackageNames, debianSourcePackageName)
import Debian.Debianize (CabalT, CabalInfo)
import Debian.GHC (hvrCompilerPATH, withModifiedPATH)
import Debian.Pretty (prettyShow, ppShow)
import Debian.Relation (BinPkgName(unBinPkgName), SrcPkgName(unSrcPkgName))
import Debian.Releases (ReleaseTree, ReleaseURI{-, releaseURI-})
import qualified Debian.Repo.Fingerprint as P (RetrieveMethod(Patch, Cd, DebDir, DataFiles, Debianize'', Proc, Quilt, SourceDeb, Twice{-, Zero-}))
import Debian.Repo.LocalRepository(uploadRemote, verifyReleaseURI)
import Debian.Repo.MonadOS (MonadOS, getOS, evalMonadOS)
import Debian.Repo.MonadRepos (HasReposState, MonadRepos, runReposT, {-runReposCachedT,-} MonadReposCached)
import Debian.Repo.OSImage (osLocalMaster, osLocalCopy, osBaseDistro)
import Debian.Repo.OSKey (OSKey(_root))
import Debian.Repo.Release (Release(releaseName))
import Debian.Repo.Repo (repoReleaseInfo)
import Debian.Repo.Rsync (HasRsyncError)
import Debian.Repo.Slice (NamedSliceList(..), SliceList(slices), Slice(sliceRepoKey, sliceSource),
                          appendSliceLists, inexactPathSlices, releaseSlices, expandPPASlice)
import Debian.Repo.SourceTree (HasSourceTree, SourceTree)
import Debian.Repo.State.AptImage (withAptImage)
import Debian.Repo.State.Repository (foldRepository)
import Debian.Repo.State.Slice (repoSources, updateCacheSources)
import Debian.Repo.Top (MonadTop, TopDir(TopDir), toTop)
import Debian.Repo.LocalRepository (LocalRepository, repoRoot)
import Debian.Sources (SourceOption(SourceOption), SourceOp(OpSet), _sourceOptions)
import Debian.TH (here, Loc)
import Debian.URI(HasParseError, URI(uriPath, uriAuthority), URIAuth(uriUserInfo, uriRegName, uriPort), uriPathLens)
import Debian.VendorURI (vendorURI)
import Debian.Version(DebianVersion, parseDebianVersion', prettyDebianVersion)
import Distribution.Pretty (Pretty)
import Extra.EnvPath (rootPath)
import Extra.Except
import Extra.Lock(withLock)
import Extra.Misc(checkSuperUser)
import Extra.Process (runV2)
import Extra.Verbosity (ePutStrLn, ePutStr, qPutStrLn, qPutStr, withVerbosity, noisier)
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

main ::
    forall e. (Exception e, Pretty e, HasLoc e, HasIOException e, HasParseError e, HasRsyncError e)
    => CabalT IO ()
    -> (ReleaseTree -> Either e ReleaseURI)
    -> (FilePath -> String -> R.ParamRec)
    -> IO [e]
main ini myBuildURI myParams =
    do ePutStrLn "Autobuilder starting..."
       args <- getArgs
       home <- getEnv "HOME"
       -- Compute all the ParamRecs implied by the command line
       -- argument, using myParams to create each default ParamRec
       -- value.
       let recs :: [R.ParamRec]
           recs = R.getParams args (myParams home)
       when (any R.doHelp recs) (ePutStr (R.usage "Usage: ") >> exitWith ExitSuccess)
       tops <- nub <$> mapM P.computeTopDir recs
       case tops of
         -- All the work for a given run must occur in the same top
         -- directory - ~/.autobuilder for example.
         [top] -> do
           (results :: [Either e (Maybe (ExitCode, L.ByteString, L.ByteString))])
               <- runReaderT (runReposT (mapM (runExceptT . doParameterSet myBuildURI ini) recs)) (TopDir top)
           testResults results
         -- [top] -> (undefined :: IO [Either e (ExitCode, L.ByteString, L.ByteString)]) >>= testResults
         [] -> ePutStrLn "No parameter sets" >> return []
         tops' -> ePutStrLn ("Parameter sets have different top directories: " ++ show tops') >> return []
    where -- foo :: FilePath -> [R.ParamRec] -> IO [Either e (ExitCode, L.ByteString, L.ByteString)]
          -- foo top recs = runReposCachedT (TopDir top) (mapM (runExceptT . doParameterSet myBuildURI ini) recs)
          testResults :: [Either e (Maybe (ExitCode, L.ByteString, L.ByteString))] -> IO [e]
          testResults results =
              let (es, results') = partitionEithers results in
              case es of
                [] -> ePutStrLn (intercalate "\n  " ("Results:" : map (uncurry showResult) (zip [(1 :: Int)..] results'))) >> return []
                es -> ePutStrLn (intercalate "\n  " ("Errors:" : map prettyShow es)) >> return es
          showResult :: Int -> Maybe (ExitCode, L.ByteString, L.ByteString) -> String
          showResult num result = "Parameter set " ++ show num ++ " Ok"
          -- showAndThrow (e :: SomeException) = ePutStrLn ("Exception: " ++ show e) >> throwM e

#if 0
partitionFailing :: [Either e a] -> ([[String]], [a])
partitionFailing xs = p ([], []) xs
    where p result [] = result
          p (fs, ss) (Failure f : more) = p (f : fs, ss) more
          p (fs, ss) (Success s : more) = p (fs, s : ss) more

isFailure :: Failing a -> Bool
isFailure (Failure _) = True
isFailure _ = False
#endif

-- |Process one set of parameters.  Usually there is only one, but there
-- can be several which are run sequentially.  Stop on first failure.
doParameterSet ::
    forall r s e m. (MonadIOError e m, HasLoc e, Exception e, HasParseError e, HasRsyncError e,
                     HasReposState s, MonadMask m, MonadState s m, MonadTop r m,
                     HasSourceTree SourceTree m)
    => (ReleaseTree -> Either e ReleaseURI)
    -> CabalT IO ()
    -> R.ParamRec
    -> m (Maybe (ExitCode, L.ByteString, L.ByteString))
-- If one parameter set fails, don't try the rest.  Not sure if
-- this is the right thing, but it is safe.
doParameterSet myBuildURI ini params = do
  withVerbosity (R.verbosity params) $ do
    TopDir top <- view toTop
    withLock (top </> "lockfile") $
      -- Should we just let the autobuilder deduce from current $PATH?
      -- Probably not, because packages built with ghc are difficult to
      -- distinguish from those built with ghc-8.0.1 as things stand.  But then
      -- we need our --hvr-version option back.
      withModifiedPATH (maybe id hvrCompilerPATH (R.hvrVersion params)) $
        P.buildCache myBuildURI params >>= runParameterSet ini

-- | Get the sources.list for the local upload repository associated
-- with the OSImage.  The resulting paths are for running inside the
-- build environment.
getLocalSources :: (MonadIOError e m, HasLoc e, MonadOS r s m) => [Loc] -> m SliceList
getLocalSources locs = do
  root <- view (osLocalCopy . repoRoot) <$> getOS
  ePutStrLn ("getLocalSources root=" <> show root <> " at " <> prettyShow ($here : locs))
  --case parseURI ("file://" ++ outsidePath root) of
    --Nothing -> error $ "Invalid local repo root: " ++ show root
    --Just uri -> do
        --ePutStrLn ("getLocalSources uri=" <> show uri <> " at " <> prettyShow ($here : locs))
  repoSources ($here : locs) [SourceOption "trusted" OpSet ["yes"]] root {-(Just (view envRoot root)) (review vendorURI uri)-}

runParameterSet ::
    forall r s e m. (MonadIOError e m, HasLoc e, MonadMask m, Exception e, HasRsyncError e, HasParseError e, MonadReposCached r s m, HasSourceTree SourceTree m)
    => CabalT IO () -> C.CacheRec -> m (Maybe (ExitCode, L.ByteString, L.ByteString))
runParameterSet ini cache =
    do
      (TopDir top) <- view toTop
      liftIO doRequiredVersion
      doVerifyBuildRepo cache
      when (R.showParams params) (withVerbosity 1 (liftIO doShowParams))
      when (R.showSources params) (withVerbosity 1 (liftIO doShowSources))
      when (R.flushAll params) (liftIO $ doFlush top)
      liftIO checkPermissions
      either (\_ -> return ()) (verifyReleaseURI (R.doSSHExport $ params)) (R.theReleaseURI params)
      qPutStrLn "Preparing dependency environment"
      extraSlices <- mapM (either (return . (: [])) (liftIO . expandPPASlice (P.baseRelease params))) (R.extraRepos params) >>= return . concat
      dependOS <- prepareDependOS params buildRelease extraSlices
      let allTargets :: [(P.RetrieveMethod, [P.PackageFlag], [CabalInfo -> CabalInfo])]
          allTargets = map (\i -> f (R.knownPackages (C.params cache) ! i)) (Set.toList (R.buildPackages params))
          f p = (view P.spec p, nub (view P.flags p), view P.post p)
      -- let allTargets = filter (notZero . view _1) (P.foldPackages (\ p l -> (view P.spec p, view P.flags p, view P.post p) : l) (R.buildPackages params) [])
      qPutStrLn "Preparing build environment"
      buildOS <- evalMonadOS (do sources <- view osBaseDistro <$> getOS
                                 updateCacheSources [$here] (R.ifSourcesChanged params) sources
                                 when (R.report params) (ePutStrLn . doReport $ allTargets)
                                 qPutStr ("\n" ++ showTargets allTargets ++ "\n")
                                 getOS >>= prepareBuildOS (R.buildRelease params)) dependOS
      ePutStrLn ("runParameterSet buildOS=" <> show buildOS <> " at " <> prettyShow $here)
      qPutStrLn "Retrieving all source code:\n"
      (failures, targets) <- partitionEithers <$> (mapM (uncurry (retrieveTarget dependOS (length allTargets))) (zip [1..] allTargets))
      ePutStrLn ("runParameterSet " <> prettyShow $here)
      when (not $ List.null $ failures) (error $ unlines $ "Some targets could not be retrieved:" : map ("  " ++) failures)

      -- Compute a list of sources for all the releases in the repository we will upload to,
      -- used to avoid creating package versions that already exist.  Also include the sources
      -- for the local repository to avoid collisions there as well.
      localSources <- evalMonadOS (getLocalSources [$here]) buildOS
      ePutStrLn ("runParameterSet localSources=" <> show localSources <> " at " <> prettyShow $here)
      local <- evalMonadOS (view osLocalMaster <$> getOS) dependOS
      let poolSources = NamedSliceList { sliceListName = parseCodename (codename (sliceListName buildRelease) <> "-all")
                                       , sliceList = appendSliceLists [buildRepoSources, localSources] }

      withAptImage [$here] (R.ifSourcesChanged params) poolSources $ do
        buildResult <- buildTargets cache dependOS buildOS local targets
        (uploadResult :: [(Either e (ExitCode, L.ByteString, L.ByteString))]) <- lift $ noisier 1 $ upload buildResult
        newDist (partitionEithers uploadResult)
    where
{-
      partitionFailing :: [Either e a] -> ([e], [a])
      partitionFailing xs =
          first concat $
          partitionEithers $
          map (\ x -> case x of
                        (Failure ms) -> Left ms
                        (Success a) -> Right a) xs
-}
      --notZero x = null (listify (\ x -> case x of P.Zero -> True; _ -> False) x)
      retrieveTarget :: OSKey -> Int -> Int -> (P.RetrieveMethod, [P.PackageFlag], [CabalInfo -> CabalInfo]) -> m (Either String (Buildable SomeDownload))
      retrieveTarget dependOS count index (method', flags', functions) = do
            liftIO (hPutStr stderr (printf "[%2d of %2d]" index count))
            res <- (Right <$> evalMonadOS (do download <- withProcAndSys (view (to _root . rootPath) dependOS) $ retrieve ini cache method' flags' functions
                                              when (R.flushSource params) (flushSource download)
                                              buildable <- liftIO (asBuildable download)
                                              let (src, bins) = debianPackageNames (debianSourceTree buildable)
                                              liftIO (hPutStrLn stderr (printf " %s - %s:" (unSrcPkgName src) (limit 100 (show method') :: String)))
                                              qPutStrLn $ "Binary debs: [" <> intercalate ", " (map unBinPkgName bins) <> "]"
                                              return buildable) dependOS) `catch` handleRetrieveException method'
            return res
      params = C.params cache
      baseRelease =  either (\e -> error $ "Could not find slice " ++ show (P.baseRelease params) ++ ": " ++ show e) id (P.findSlice [$here] cache (P.baseRelease params))
      buildRepoSources = C.buildRepoSources cache
      buildReleaseSources = releaseSlices (R.buildRelease params) (inexactPathSlices buildRepoSources)
      buildRelease = NamedSliceList { sliceListName = R.buildRelease params
                                    , sliceList = appendSliceLists [sliceList baseRelease, setOptions [SourceOption "trusted" OpSet ["yes"]] buildReleaseSources] }
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
          either (error . show) doShow (P.findSlice [$here] cache (R.buildRelease params))
          where
            doShow sources =
                do qPutStrLn $ (codename . sliceListName $ sources) ++ ":"
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
      upload :: (LocalRepository, [Target SomeDownload]) -> m [Either e (ExitCode, L.ByteString, L.ByteString)]
      upload (repo, [])
          | R.doUpload params =
              case R.theVendorURI params of
                Left e -> error "Cannot upload, no 'Upload-URI' parameter given"
                Right uri -> do
                  qPutStrLn "Uploading from local repository to remote"
                  fmap fooFailing <$> uploadRemote repo uri
          | True = return []
      upload (_, failed) =
          do let msg = ("Some targets failed to build:\n  " ++ intercalate "\n  " (map (ppShow . debianSourcePackageName) failed))
             qPutStrLn msg
             case R.doUpload params of
               True -> qPutStrLn "Skipping upload."
               False -> return ()
             error msg
      newDist ::
          forall e' m'. (MonadIOError e' m', Show e', MonadCatch m')
          => ([e'], [(ExitCode, L.ByteString, L.ByteString)])
          -> m' (Maybe (ExitCode, L.ByteString, L.ByteString))
      newDist ([], _)
          | R.doNewDist params =
              case R.theVendorURI params of
                Right uri ->
                    do let p = case view (vendorURI . to uriAuthority) uri of
                                 Just auth ->
                                     let cmd = "ssh"
                                         args = [uriUserInfo auth ++ uriRegName auth, R.newDistProgram params,
                                                 "--sign", "--root", view (vendorURI . uriPathLens) uri] ++
                                                (concat . map (\ rel -> ["--create", rel]) . R.createRelease $ params) in
                                     (proc cmd args)
                                 _ ->
                                     let cmd = R.newDistProgram params
                                         args = ["--sign", "--root", view (vendorURI . uriPathLens) uri] in
                                     (proc cmd args)
                       qPutStrLn (" -> " ++ showCmdSpecForUser (cmdspec p))
                       testOutput =<< runV2 [$here] p L.empty
{-
                       -- result <- (undefined :: (Either IOException (ExitCode, L.ByteString, L.ByteString)) -> (Either e' (Maybe (ExitCode, L.ByteString, L.ByteString)))) <$> liftEIO $here (runVE2 $here p L.empty)
                       case result of
                         (Right _) -> return result
                         (Left e) -> ePutStrLn ("newdist failed:" <> show e) >> return result
-}
                       -- return result
                Left e -> throwError $ fromIOException $ userError "Missing Upload-URI parameter"
          | True = return Nothing
      newDist (es, _) = throwError $ fromIOException $ userError (unlines (fmap show es))

      testOutput :: forall e' m'. MonadIOError e' m' => (ExitCode, L.ByteString, L.ByteString) -> m' (Maybe (ExitCode, L.ByteString, L.ByteString))
      testOutput result@(ExitSuccess, _, _) = return (Just result)
      testOutput (code, out, err) =
          throwError $ fromIOException (userError (show code <> "\n" <> ({-LT.unpack $ decodeUtf8 $ mconcat $ L.toChunks $ snd $ indentChunks " 1> " " 2> "-} show (out, err))))

      setOptions :: [SourceOption] -> SliceList -> SliceList
      setOptions opts l = l {slices = fmap (\s -> s {sliceSource = (sliceSource s) {_sourceOptions = opts}}) (slices l)}

fooFailing :: HasIOException e => Failing a -> Either e a
fooFailing (Failure msgs) = Left (fromIOException (userError (unlines msgs)))
fooFailing (Success r) = Right r

-- | Make sure the build release ("P.buildRelease params") - the
-- release and repository to which we intend to upload the packages
-- that we build - exists on the upload server ("P.uploadURI params").
doVerifyBuildRepo :: (MonadIOError e m, HasLoc e, MonadRepos s m) => C.CacheRec -> m ()
doVerifyBuildRepo cache =
    do repoNames <- (nub . map releaseName . concat) <$> mapM (foldRepository [$here] (return . repoReleaseInfo) (return . repoReleaseInfo)) keys
       when (not (any (== (R.buildRelease params)) repoNames))
            (case R.theVendorURI params of
               Left _e -> error "No uploadURI?"
               Right uri ->
                   let ssh = case view (vendorURI . to uriAuthority) uri of
                               Just auth -> uriUserInfo auth ++ uriRegName auth ++ uriPort auth
                               Nothing -> "user@hostname"
                       rel = codename (R.buildRelease params)
                       top = view (vendorURI . to uriPath) uri in -- "/home/autobuilder/deb-private/debian"
                   error $ intercalate "\n  "
                             [ prettyShow $here <> " - build repository " <> show (R.buildRelease params) <> " does not exist on remote server: "
                             , "vendorURI: " ++ show (R.theVendorURI params)
                             , intercalate "\n    " ("available repoNames:" : fmap show repoNames)
                             , "Use newdist there to create it:"
                             , "  ssh " ++ ssh ++ " " ++ R.newDistProgram params ++ " --root=" ++ top ++ " --create-release=" ++ rel
                             , "  ssh " ++ ssh ++ " " ++ R.newDistProgram params ++ " --root=" ++ top ++ " --create-section=" ++ rel ++ ",main"
                             , "You will also need to remove the local file ~/.autobuilder/repoCache." ])
    where
      keys = map sliceRepoKey . slices . C.buildRepoSources $ cache
      params = C.params cache

handleRetrieveException :: (MonadIO m, MonadThrow m) => P.RetrieveMethod -> SomeException -> m (Either String (Buildable SomeDownload))
handleRetrieveException method' e =
          case (fromException (toException e) :: Maybe AsyncException) of
            Just UserInterrupt ->
                throwM e -- break out of loop
            _ -> let message = ("Failure retrieving " ++ show method' ++ ":\n  " ++ show e) in
                 liftIO (ePutStrLn message) >> return (Left message)

limit n s = if length s > n + 3 then take n s ++ "..." else s

doReport :: [(P.RetrieveMethod, [P.PackageFlag], [CabalInfo -> CabalInfo])] -> String
doReport =
    intercalate "\n" . concatMap doReport'
    where
      doReport' :: (P.RetrieveMethod, [P.PackageFlag], [CabalInfo -> CabalInfo]) -> [String]
      doReport' (spec, flags', fns) =
          patched spec ++ pinned flags'
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
