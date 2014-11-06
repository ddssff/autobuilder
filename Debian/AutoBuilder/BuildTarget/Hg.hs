{-# LANGUAGE GADTs, OverloadedStrings, ScopedTypeVariables #-}
-- | A Mercurial archive.
module Debian.AutoBuilder.BuildTarget.Hg where

import Control.Exception (SomeException, try)
import Control.Monad.Trans
--import Data.ByteString.Lazy.Char8 (empty)
import qualified Debian.AutoBuilder.Types.Download as T
import qualified Debian.AutoBuilder.Types.CacheRec as P
import qualified Debian.AutoBuilder.Types.Packages as P
import Debian.Repo
import Debian.Repo.Fingerprint (RetrieveMethod)
import Debian.Repo.Prelude.Process (timeTask)
import System.Directory
import System.FilePath (splitFileName, (</>))
import System.Process (shell)
import System.Unix.Directory

documentation :: [String]
documentation = [ "hg:<string> - A target of this form target obtains the source"
                , "code by running the Mercurial command 'hg clone <string>'." ]

data HgDL
    = HgDL { cache :: P.CacheRec
           , method :: RetrieveMethod
           , flags :: [P.PackageFlag]
           , archive :: String
           , tree :: SourceTree }

instance T.Download HgDL where
    method = method
    flags = flags
    getTop = topdir . tree
    logText x = "Hg revision: " ++ show (method x)
    flushSource _ = error "HgDL flushSource unimplemented"
    cleanTarget x =
        (\ path -> case any P.isKeepRCS (flags x) of
                     False -> let cmd = "rm -rf " ++ path ++ "/.hg" in
                              timeTask (readProcFailing (shell cmd) "")
                     _ -> return ([], 0))

prepare :: (MonadRepos m, MonadTop m) => P.CacheRec -> RetrieveMethod -> [P.PackageFlag] -> String -> m T.SomeDownload
prepare cache method flags archive =
    do
      dir <- sub ("hg" </> archive)
      exists <- liftIO $ doesDirectoryExist dir
      tree <- liftIO $ if exists then verifySource dir else createSource dir
      return $ T.SomeDownload $ HgDL { cache = cache
                                     , method = method
                                     , flags = flags
                                     , archive = archive
                                     , tree = tree }
    where
      verifySource dir =
          try (readProcFailing (shell ("cd " ++ dir ++ " && hg status | grep -q .")) "") >>=
          either (\ (_ :: SomeException) -> updateSource dir)	-- failure means there were no changes
                 (\ _ -> removeSource dir >> createSource dir)	-- success means there was a change

      removeSource dir = liftIO $ removeRecursiveSafely dir

      updateSource dir =
          readProcFailing (shell ("cd " ++ dir ++ " && hg pull -u")) "" >>
          findSourceTree dir :: IO SourceTree

      createSource dir =
          let (parent, _) = splitFileName dir in
          liftIO (createDirectoryIfMissing True parent) >>
          readProcFailing (shell ("hg clone " ++ archive ++ " " ++ dir)) "" >>
          findSourceTree dir :: IO SourceTree
