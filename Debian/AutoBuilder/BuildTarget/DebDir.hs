{-# LANGUAGE CPP, GADTs, ScopedTypeVariables #-}
module Debian.AutoBuilder.BuildTarget.DebDir
    ( documentation
    , prepare
    ) where

import Control.Monad.Trans (liftIO)
import Data.Set (union)
#if MIN_VERSION_Cabal(2,0,0)
import Distribution.Version (showVersion)
#else
import Data.Version (showVersion)
#endif
import Debian.AutoBuilder.Types.Download as T
import qualified Debian.AutoBuilder.Types.Packages as P
import Debian.Changes (logVersion)
import Debian.Repo
import Debian.Repo.Fingerprint (RetrieveMethod, retrieveMethodMD5)
import Debian.Repo.Rsync (rsyncOld)
import Debian.Version (version)
import System.Directory
import System.FilePath ((</>), takeBaseName)

documentation = [ "deb-dir:(<target>):(<target>) - A target of this form combines two targets,"
                , "where one points to an un-debianized source tree and the other contains"
                , "a debian subdirectory." ]

data DebDirDL a b
    = DebDirDL { ddMethod :: RetrieveMethod
               , ddFlags :: [P.PackageFlag]
               , upstream :: a
               , debian :: b
               , tree :: DebianSourceTree } deriving Show

instance (Download a, Download b) => Download (DebDirDL a b) where
    method = ddMethod
    flags = ddFlags
    getTop = topdir . tree
    logText x = "deb-dir revision: " ++ show (method x)
    origTarball = origTarball . upstream
    flushSource = flushSource . upstream
    attrs x = union (attrs (upstream x)) (attrs (debian x))

prepare :: (MonadRepos s m, MonadTop r m, T.Download a, T.Download b) =>
           RetrieveMethod -> [P.PackageFlag] -> a -> b -> m SomeDownload
prepare method flags upstream debian = do
  dir <- sub "deb-dir"
  dest <- sub ("deb-dir" </> retrieveMethodMD5 method)
  liftIO (createDirectoryIfMissing True dir)
  rsyncOld [] (T.getTop upstream) dest
  let debianSubdir = case takeBaseName (T.getTop debian) of
                       "debian" -> T.getTop debian
                       _ -> (T.getTop debian </> "debian")
  rsyncOld [] debianSubdir (dest </> "debian")
  tree <- liftIO (findSourceTree dest :: IO DebianSourceTree)

  let tgt = DebDirDL {ddMethod = method, ddFlags = flags, upstream = SomeDownload upstream, debian = SomeDownload debian, tree = tree}
  -- The upstream and downstream versions must match after the epoch and revision is stripped.
  case T.mVersion upstream of
    Nothing -> return $ SomeDownload tgt
    Just upstreamV ->
        let debianV = logVersion (entry tree) in
        case compare (version debianV) (showVersion upstreamV) of
          -- If the debian version is too old it needs to be bumped, this ensures we notice
          -- when a new upstream appears.  We should just modify the changelog directly.
          LT -> error $ show method ++ ": version in Debian changelog (" ++ version debianV ++ ") is too old for the upstream (" ++ showVersion upstreamV ++ ")"
          _ -> return $ SomeDownload tgt
