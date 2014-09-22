-- | Copy the target and apply a patch.
{-# LANGUAGE GADTs, Rank2Types, ScopedTypeVariables #-}
module Debian.AutoBuilder.BuildTarget.Patch where

import qualified Debug.Trace as D

import Control.Exception (SomeException, try)
import Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Char8 as B
import qualified Debian.AutoBuilder.Types.Download as T
import qualified Debian.AutoBuilder.Types.Packages as P
import Debian.Repo (findSourceTree, copySourceTree, SourceTree(dir'), DebianSourceTree, findDebianSourceTrees, sub, MonadRepos, MonadTop)
import Debian.Repo.Fingerprint (RetrieveMethod(Apt, Patch), retrieveMethodMD5)
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode(ExitSuccess, ExitFailure))
import System.FilePath ((</>))
import System.Process (proc, CreateProcess(cwd), showCommandForUser)
import System.Process.ByteString.Lazy ()
import System.Process.ByteString ()
import System.Process.ListLike (readCreateProcessWithExitCode)

{-
-- |Scan the flag list for Patch flag, and apply the patches
patch :: P.CacheRec -> [P.PackageFlag] -> String -> Version -> IO ()
patch cache flags name version =
    mapM_ patch' flags
    where
      patch' :: P.PackageFlag -> IO ()
      patch' (P.Patch text) =
          do (_out, err, res) <- lazyProcessE "/usr/bin/patch" ["-p1"] (Just (unpacked cache name version)) Nothing text >>=
                                 return . collectOutputUnpacked
             case res of
               ExitFailure n -> error ("patch " ++ show (unpacked cache name version) ++ " -> " ++
                                       show n ++ "\noutput: " ++ err ++ "\npatch:\n" ++ B.unpack text)
               ExitSuccess -> return ()
      patch' _ = return ()

instance Show Patch where
    show (Patch t) = "patch:" ++ show t
-}

documentation :: [String]
documentation = [ "Patch <target> <patchtext> - Apply the patch to the target." ]

data T.Download a => PatchDL a
    = PatchDL { method :: RetrieveMethod
              , flags :: [P.PackageFlag]
              , patch :: B.ByteString
              , base :: a
              , tree :: SourceTree }

instance T.Download a => T.Download (PatchDL a) where
    method = method
    flags = flags
    getTop = dir' . tree
    logText x = T.logText (base x) ++ " (with patch applied)"
    flushSource x = T.flushSource (base x)
    cleanTarget x = T.cleanTarget (base x)
    attrs = T.attrs . base

prepare :: (MonadRepos m, MonadTop m, T.Download a) => RetrieveMethod -> [P.PackageFlag] -> B.ByteString -> a -> m T.SomeDownload
prepare method flags patch base =
    do copyDir <- sub ("quilt" </> retrieveMethodMD5 method)
       baseTree <- liftIO $ findSourceTree (T.getTop base)
       liftIO $ createDirectoryIfMissing True copyDir
       tree <- liftIO $ copySourceTree baseTree copyDir
       subDir <- liftIO $ findSource method copyDir
       (res, out, err) <- liftIO $ readCreateProcessWithExitCode ((proc cmd args) {cwd = Just subDir}) patch
       case res of
         ExitFailure _ -> error (showCommandForUser cmd args ++ " -> " ++ show res ++
                                 "\ncwd:" ++ subDir ++
                                 "\nstdout:\n" ++ indent (B.unpack out) ++
                                 "\nstderr:\n" ++ indent (B.unpack err) ++
                                 "\npatch:\n" ++ indent (B.unpack patch))
         ExitSuccess ->
             return $ T.SomeDownload $ PatchDL {method = method, flags = flags, patch = patch, base = T.SomeDownload base, tree = tree}
    where
      cmd = "/usr/bin/patch"
      args = ["-p1"]

indent :: String -> String
indent = unlines . map (" > " ++) . lines

-- | For the Apt target, the real source tree is in a subdirctory.
findSource :: RetrieveMethod -> FilePath -> IO FilePath
findSource (Patch (Apt _dist _name) _) copyDir =
  try (findDebianSourceTrees (D.trace ("findDebianSourceTree " ++ show copyDir) copyDir)) >>=
  return . either (\ (e :: SomeException) -> D.trace (" -> " ++ show e) copyDir)
           (\ (ts :: [(FilePath, DebianSourceTree)]) ->
             case ts of
               [(subdir, _)] -> D.trace (" -> " ++ show (copyDir </> subdir)) (copyDir </> subdir)
               [] -> error "findSource: Internal error"
               _ -> error $ "Multiple debian source trees in " ++ copyDir ++ ": " ++ show (map fst ts))
findSource _ copyDir = return copyDir

{-
instance BuildTarget Patch where
    getTop params (Patch t _) = getTop params t
    cleanTarget params (Patch t _) source = cleanTarget params t source
    -- We can't include the whole patch text in the revision string.
    revision params (Patch t _) =  
        Debian.AutoBuilder.BuildTarget.Common.revision params t >>= return . ("patch:" ++)
    buildWrapper _params buildOS _buildTree _status _target action = withProc buildOS action
    logText (Proc s) revision = logText s revision ++ " (with /proc mounted)"

prepare :: MonadApt m => P.CacheRec -> Tgt -> String -> m Patch
prepare cache base patch = return $ Patch base patch

-- |Scan the flag list for Patch flag, and apply the patches
patch :: FilePath -> Patch [P.PackageFlag] -> String -> DebianVersion -> IO ()
patch top flags name version =
    mapM_ patch' flags
    where
      patch' :: P.PackageFlag -> IO ()
      patch' (P.Patch text) =
          do (_out, err, res) <- lazyProcessE "/usr/bin/patch" ["-p1"] (Just (unpacked top name version)) Nothing text >>=
                                 return . collectOutputUnpacked
             case res of
               ExitFailure n -> error ("Applying patch to " ++ unpacked top name version ++ " -> " ++
                                       show n ++ "\noutput: " ++ err ++ "\npatch:\n" ++ B.unpack text)
               ExitSuccess -> return ()
      patch' _ = return ()
-}
