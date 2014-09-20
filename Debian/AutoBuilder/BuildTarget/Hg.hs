{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
-- | A Mercurial archive.
module Debian.AutoBuilder.BuildTarget.Hg where

import Control.Exception (SomeException, try)
import Control.Monad
import Control.Monad.Trans
import Data.Set (empty)
--import Data.ByteString.Lazy.Char8 (empty)
import qualified Debian.AutoBuilder.Types.Download as T
import qualified Debian.AutoBuilder.Types.CacheRec as P
import qualified Debian.AutoBuilder.Types.Packages as P
import qualified Debian.AutoBuilder.Types.ParamRec as P
import Debian.Repo
import Debian.Repo.Fingerprint (RetrieveMethod)
import Debian.Repo.Prelude.Verbosity (timeTask)
import System.Directory
import System.FilePath (splitFileName, (</>))
import System.Process (shell)
import System.Unix.Directory

documentation :: [String]
documentation = [ "hg:<string> - A target of this form target obtains the source"
                , "code by running the Mercurial command 'hg clone <string>'." ]

prepare :: (MonadRepos m, MonadTop m) => P.CacheRec -> RetrieveMethod -> [P.PackageFlag] -> String -> m T.Download
prepare cache method flags archive =
    do
      dir <- sub ("hg" </> archive)
      when (P.flushSource (P.params cache)) (liftIO $ removeRecursiveSafely dir)
      exists <- liftIO $ doesDirectoryExist dir
      tree <- liftIO $ if exists then verifySource dir else createSource dir
      return $ T.Download { T.method = method
                          , T.flags = flags
                          , T.getTop = topdir tree
                          , T.logText =  "Hg revision: " ++ show method
                          , T.mVersion = Nothing
                          , T.origTarball = Nothing
                          , T.cleanTarget =
                              \ path -> case any P.isKeepRCS flags of
                                          False -> let cmd = "rm -rf " ++ path ++ "/.hg" in
                                                   timeTask (readProcFailing (shell cmd) "")
                                          _ -> return ([], 0)
                          , T.buildWrapper = id
                          , T.attrs = empty
                          }
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
