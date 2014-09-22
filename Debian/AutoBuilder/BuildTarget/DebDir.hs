{-# LANGUAGE CPP, GADTs, ScopedTypeVariables #-}
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

data (Download a, Download b) => DebDirDL a b
    = DebDirDL { ddMethod :: RetrieveMethod
               , ddFlags :: [P.PackageFlag]
               , upstream :: a
               , debian :: b
               , tree :: DebianSourceTree }

instance (Download a, Download b) => Download (DebDirDL a b) where
    method = ddMethod
    flags = ddFlags
    getTop = topdir . tree
    logText x = "deb-dir revision: " ++ show (method x)
    origTarball = origTarball . upstream
    flushSource = flushSource . upstream
    attrs x = union (attrs (upstream x)) (attrs (debian x))

prepare :: (MonadRepos m, MonadTop m, T.Download a, T.Download b) =>
           RetrieveMethod -> [P.PackageFlag] -> a -> b -> m SomeDownload
prepare method flags upstream debian =
    sub "deb-dir" >>= \ dir ->
    sub ("deb-dir" </> show (md5 (pack (show method)))) >>= \ dest ->
    liftIO (createDirectoryIfMissing True dir) >>
    rsync [] (T.getTop upstream) dest >>
    rsync [] (T.getTop debian </> "debian") (dest </> "debian") >>
    liftIO (findSourceTree dest :: IO DebianSourceTree) >>= \ tree ->

    let tgt = DebDirDL {ddMethod = method, ddFlags = flags, upstream = SomeDownload upstream, debian = SomeDownload debian, tree = tree} in
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
