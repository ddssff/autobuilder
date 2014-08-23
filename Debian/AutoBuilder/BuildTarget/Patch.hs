-- | Copy the target and apply a patch.
{-# LANGUAGE Rank2Types, ScopedTypeVariables #-}
module Debian.AutoBuilder.BuildTarget.Patch where

import qualified Debug.Trace as D

import Control.Exception (SomeException, try)
import Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Digest.Pure.MD5 (md5)
import qualified Debian.AutoBuilder.Types.Download as T
import qualified Debian.AutoBuilder.Types.Packages as P
import Debian.Repo (findSourceTree, copySourceTree, SourceTree(dir'), DebianSourceTree, findDebianSourceTrees, sub, MonadRepos, MonadTop)
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode(ExitSuccess, ExitFailure))
import System.FilePath ((</>))
import System.Process (proc, CreateProcess(cwd), showCommandForUser)
import System.Process.Read (readCreateProcessWithExitCode)

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

prepare :: (MonadRepos m, MonadTop m) => P.Packages -> B.ByteString -> T.Download -> m T.Download
prepare package patch base =
    do copyDir <- sub ("quilt" </> show (md5 (L.pack (show (P.spec package)))))
       baseTree <- liftIO $ findSourceTree (T.getTop base)
       liftIO $ createDirectoryIfMissing True copyDir
       tree <- liftIO $ copySourceTree baseTree copyDir
       subDir <- liftIO $ findSource (P.spec package) copyDir
       (res, out, err) <- liftIO $ readCreateProcessWithExitCode ((proc cmd args) {cwd = Just subDir}) patch
       case res of
         ExitFailure _ -> error (showCommandForUser cmd args ++ " -> " ++ show res ++
                                 "\ncwd:" ++ subDir ++
                                 "\nstdout:\n" ++ indent (B.unpack out) ++
                                 "\nstderr:\n" ++ indent (B.unpack err) ++
                                 "\npatch:\n" ++ indent (B.unpack patch))
         ExitSuccess ->
             return $ T.Download {
                          T.package = package
                        , T.getTop = dir' tree
                        , T.logText = T.logText base ++ " (with patch applied)"
                        , T.mVersion = Nothing
                        , T.origTarball = Nothing
                        , T.cleanTarget = T.cleanTarget base
                        , T.buildWrapper = id
                        , T.attrs = T.attrs base
                        }
    where
      cmd = "/usr/bin/patch"
      args = ["-p1"]

indent :: String -> String
indent = unlines . map (" > " ++) . lines

-- | For the Apt target, the real source tree is in a subdirctory.
findSource :: P.RetrieveMethod -> FilePath -> IO FilePath
findSource (P.Patch (P.Apt _dist _name) _) copyDir =
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
