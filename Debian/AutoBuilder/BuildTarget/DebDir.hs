{-# LANGUAGE CPP, ScopedTypeVariables #-}
module Debian.AutoBuilder.BuildTarget.DebDir
    ( documentation
    , prepare
    ) where

import Control.Monad.Trans (liftIO)
import Data.ByteString.Lazy.Char8 (pack)
import Data.Digest.Pure.MD5 (md5)
import Data.Set (union)
import Data.Version (showVersion)
import Debian.AutoBuilder.Types.Download as T
import qualified Debian.AutoBuilder.Types.Packages as P
import Debian.Changes (logVersion)
import Debian.Repo
import Debian.Repo.Fingerprint (RetrieveMethod)
import Debian.Version (version)
import System.Directory
import System.FilePath ((</>))

documentation = [ "deb-dir:(<target>):(<target>) - A target of this form combines two targets,"
                , "where one points to an un-debianized source tree and the other contains"
                , "a debian subdirectory." ]

prepare :: (MonadRepos m, MonadTop m) => RetrieveMethod -> [P.PackageFlag] -> T.Download -> T.Download -> m T.Download
prepare method flags upstream debian =
    sub "deb-dir" >>= \ dir ->
    sub ("deb-dir" </> show (md5 (pack (show method)))) >>= \ dest ->
    liftIO (createDirectoryIfMissing True dir) >>
    rsync [] (T.getTop upstream) dest >>
    rsync [] (T.getTop debian </> "debian") (dest </> "debian") >>
    liftIO (findSourceTree dest :: IO DebianSourceTree) >>= \ tree ->
    let tgt = T.download'
              {-  T.method = -} method
              {- , T.flags = -} flags
              {- , T.getTop = -} (topdir tree)
              {- , T.logText = -} ("deb-dir revision: " ++ show method)
              {- , T.mVersion = -} Nothing
              {- , T.origTarball = -} (T.origTarball upstream)
              {- , T.cleanTarget = -} (\ _ -> return ([], 0))
              {- , T.buildWrapper = -} id
              {- , T.attrs = -} (union (T.attrs upstream) (T.attrs debian))
              in
    -- The upstream and downstream versions must match after the epoch and revision is stripped.
    case T.mVersion upstream of
      Nothing -> return tgt
      Just upstreamV ->
          let debianV = logVersion (entry tree) in
          case compare (version debianV) (showVersion upstreamV) of
            -- If the debian version is too old it needs to be bumped, this ensures we notice
            -- when a new upstream appears.  We should just modify the changelog directly.
            LT -> error $ show method ++ ": version in Debian changelog (" ++ version debianV ++ ") is too old for the upstream (" ++ showVersion upstreamV ++ ")"
            _ -> return tgt
