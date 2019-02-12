{-# LANGUAGE CPP, GADTs, OverloadedStrings, ScopedTypeVariables, TemplateHaskell #-}
-- | A Mercurial archive.
module Debian.AutoBuilder.BuildTarget.Hg where

import Control.Exception (SomeException, try)
import Control.Monad.Trans
import qualified Data.ByteString.Lazy as B
import qualified Debian.AutoBuilder.Types.Download as T
import qualified Debian.AutoBuilder.Types.CacheRec as P
import qualified Debian.AutoBuilder.Types.Packages as P
--import Debian.Except (runExceptT)
import Debian.Repo
import Debian.Repo.Fingerprint (RetrieveMethod)
import Debian.Repo.Prelude.Process (runVE2, runV2, timeTask)
import Debian.TH (here)
import System.Directory
import System.FilePath (splitFileName, (</>))
import System.Process (shell)
import System.Unix.Directory

documentation :: [String]
documentation = [ "hg:<string> - A target of this form target obtains the source"
                , "code by running the Mercurial command 'hg clone <string>'." ]

data HgDL
    = HgDL { _cache :: P.CacheRec
           , _method :: RetrieveMethod
           , _flags :: [P.PackageFlag]
           , _archive :: String
           , _tree :: SourceTree } deriving Show

instance T.Download HgDL where
    method = _method
    flags = _flags
    getTop = topdir . _tree
    logText x = "Hg revision: " ++ show (_method x)
    flushSource _ = error "HgDL flushSource unimplemented"
    cleanTarget x =
        (\ path -> case any P.isKeepRCS (_flags x) of
                     False -> let cmd = "rm -rf " ++ path ++ "/.hg" in
                              timeTask (runVE2 [$here] (shell cmd) B.empty)
                     _ -> return (Right mempty, 0))

prepare :: (MonadIO m, MonadTop r m) => P.CacheRec -> RetrieveMethod -> [P.PackageFlag] -> String -> m T.SomeDownload
prepare cache method flags archive =
    do
      dir <- sub ("hg" </> archive)
      exists <- liftIO $ doesDirectoryExist dir
      tree <- liftIO $ if exists then verifySource dir else createSource dir
      return $ T.SomeDownload $ HgDL { _cache = cache
                                     , _method = method
                                     , _flags = flags
                                     , _archive = archive
                                     , _tree = tree }
    where
      verifySource dir =
          try (runV2 [$here] (shell ("cd " ++ dir ++ " && hg status | grep -q .")) B.empty) >>=
          either (\ (_ :: SomeException) -> updateSource dir)   -- failure means there were no changes
                 (\ _ -> removeSource dir >> createSource dir)  -- success means there was a change

      removeSource dir = liftIO $ removeRecursiveSafely dir

      updateSource dir =
          runV2 [$here] (shell ("cd " ++ dir ++ " && hg pull -u")) B.empty >>
          findSourceTree dir :: IO SourceTree

      createSource dir =
          let (parent, _) = splitFileName dir in
          liftIO (createDirectoryIfMissing True parent) >>
          runV2 [$here] (shell ("hg clone " ++ archive ++ " " ++ dir)) B.empty >>
          findSourceTree dir :: IO SourceTree
