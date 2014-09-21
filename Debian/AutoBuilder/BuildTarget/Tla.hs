{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Debian.AutoBuilder.BuildTarget.Tla where

import Control.Exception (SomeException, try)
import Control.Monad
import Control.Monad.Trans
--import qualified Data.ByteString.Lazy.Char8 as L
import Data.Set (empty)
import qualified Debian.AutoBuilder.Types.Download as T
import qualified Debian.AutoBuilder.Types.CacheRec as P
import qualified Debian.AutoBuilder.Types.ParamRec as P
import qualified Debian.AutoBuilder.Types.Packages as P
import Debian.Repo
import Debian.Repo.Fingerprint (RetrieveMethod)
import System.FilePath (splitFileName, (</>))
import System.Unix.Directory
import System.Process (shell)
import Debian.Repo.Prelude.Verbosity (timeTask, qPutStrLn)
import System.Directory

documentation :: [String]
documentation = [ "tla:<revision> - A target of this form retrieves the a TLA archive with the"
                , "given revision name." ]

prepare :: (MonadRepos m, MonadTop m) => P.CacheRec -> RetrieveMethod -> [P.PackageFlag] -> String -> m T.Download'
prepare cache method flags version =
    do
      dir <- sub ("tla" </> version)
      when (P.flushSource (P.params cache)) (liftIO (removeRecursiveSafely dir))
      exists <- liftIO $ doesDirectoryExist dir
      tree <- liftIO $ if exists then verifySource dir else createSource dir
      return $ T.download'{-   T.method = -} method
                          {- , T.flags = -} flags
                          {- , T.getTop = -} (topdir tree)
                          {- , T.logText = -}  ("TLA revision: " ++ show method)
                          {- , T.mVersion = -} Nothing
                          {- , T.origTarball = -} Nothing
                          {- , T.cleanTarget = -}
                              (\ path ->
                                  case any P.isKeepRCS flags of
                                    False -> let cmd = "find '" ++ path ++ "' -name '.arch-ids' -o -name '{arch}' -prune | xargs rm -rf" in
                                             timeTask (readProcFailing (shell cmd) "")
                                    True -> (return ([], 0)))
                          {- , T.buildWrapper = -} id
                          {- , T.attrs = -} empty
    where
      verifySource dir =
          do result <- try (readProcFailing (shell ("cd " ++ dir ++ " && tla changes")) "")
             case result of
               Left (e :: SomeException) -> qPutStrLn (show e) >> removeSource dir >> createSource dir -- Failure means there is corruption
               Right _output -> updateSource dir						         -- Success means no changes

      removeSource dir = liftIO $ removeRecursiveSafely dir

      updateSource dir =
          readProcFailing (shell ("cd " ++ dir ++ " && tla update " ++ version)) "" >>
             -- At one point we did a tla undo here.  However, we are
             -- going to assume that the "clean" copies in the cache
             -- directory are clean, since some of the other target
             -- types have no way of doing this reversion.
          findSourceTree dir :: IO SourceTree

      createSource dir =
          do
            -- Create parent dir and let tla create dir
            let (parent, _) = splitFileName dir
            liftIO $ createDirectoryIfMissing True parent
            _output <- readProcFailing (shell ("tla get " ++ version ++ " " ++ dir)) ""
            findSourceTree dir
