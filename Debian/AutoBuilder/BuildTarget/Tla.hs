{-# LANGUAGE CPP, OverloadedStrings, ScopedTypeVariables, TemplateHaskell #-}
module Debian.AutoBuilder.BuildTarget.Tla where

import Control.Exception (IOException, SomeException, try)
import Control.Monad.Trans
import qualified Data.ByteString.Lazy as B
import qualified Debian.AutoBuilder.Types.Download as T
import qualified Debian.AutoBuilder.Types.CacheRec as P
import qualified Debian.AutoBuilder.Types.Packages as P
import Debian.Repo
import Debian.Repo.Fingerprint (RetrieveMethod)
import Debian.Repo.Prelude.Process (runV2, runVE2, timeTask)
import Debian.Repo.Prelude.Verbosity (qPutStrLn)
import Debian.TH (here)
import System.Exit (ExitCode)
import System.FilePath (splitFileName, (</>))
import System.Process (shell)
import System.Unix.Directory
import System.Directory

documentation :: [String]
documentation = [ "tla:<revision> - A target of this form retrieves the a TLA archive with the"
                , "given revision name." ]

data TlaDL
    = TlaDL
      { cache :: P.CacheRec
      , method :: RetrieveMethod
      , flags :: [P.PackageFlag]
      , version :: String
      , tree :: SourceTree
      } deriving Show

instance T.Download TlaDL where
    method = method
    flags = flags
    getTop = topdir . tree
    logText x = "TLA revision: " ++ show (method x)
    flushSource _ = error "TlaDL flushSource unimplemented"
    cleanTarget x = (\ path ->
                         case any P.isKeepRCS (flags x) of
                           False -> let cmd = "find '" ++ path ++ "' -name '.arch-ids' -o -name '{arch}' -prune | xargs rm -rf" in
                                    timeTask (runVE2 [$here] (shell cmd) "" :: IO (Either IOException (ExitCode, B.ByteString, B.ByteString)))
                           True -> (return (Right mempty, 0)))

prepare :: (MonadIO m, MonadTop r m) => P.CacheRec -> RetrieveMethod -> [P.PackageFlag] -> String -> m T.SomeDownload
prepare cache' method' flags' version' =
    do
      dir <- sub ("tla" </> version')
      exists <- liftIO $ doesDirectoryExist dir
      tree'' <- liftIO $ if exists then verifySource dir else createSource dir
      return $ T.SomeDownload $ TlaDL { cache = cache'
                                      , method = method'
                                      , flags = flags'
                                      , version = version'
                                      , tree = tree'' }
    where
      verifySource dir =
          do result <- try (runV2 [$here] (shell ("cd " ++ dir ++ " && tla changes")) B.empty)
             case result of
               Left (e :: SomeException) -> qPutStrLn (show e) >> removeSource dir >> createSource dir -- Failure means there is corruption
               Right _output -> updateSource dir                                                         -- Success means no changes

      removeSource dir = liftIO $ removeRecursiveSafely dir

      updateSource dir =
          runV2 [$here] (shell ("cd " ++ dir ++ " && tla update " ++ version')) B.empty >>
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
            _output <- runV2 [$here] (shell ("tla get " ++ version' ++ " " ++ dir)) B.empty
            findSourceTree dir
