{-# LANGUAGE CPP, GADTs, OverloadedStrings, Rank2Types, ScopedTypeVariables, TemplateHaskell #-}
module Debian.AutoBuilder.BuildTarget.Darcs
    ( documentation
    , prepare
    ) where

import Control.Exception (Exception)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Except (MonadError)
import Control.Monad.Trans (liftIO, MonadIO)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Digest.Pure.MD5 (md5)
import Data.List (sort)
import Data.Maybe (mapMaybe)
import Data.Set (singleton)
import qualified Debian.AutoBuilder.Types.Download as T
import qualified Debian.AutoBuilder.Types.Packages as P
import Debian.Repo
import Debian.Repo.Fingerprint (RetrieveMethod, RetrieveAttribute(DarcsChangesId))
import Debian.TH (here)
import Extra.Process (runQE2, runVE2, runV2, timeTask)
import Network.URI (URI(..), URIAuth(..), uriToString, parseURI)
import System.Directory
import System.Exit (ExitCode(..))
import System.FilePath
import System.Process (shell, proc, CreateProcess(cwd))
import System.Unix.Directory

documentation :: [String]
documentation = [ "darcs:<string> - a target of this form obtains the source code by running"
                , "darcs get <string>.  If the argument needs to use ssh to reach the darcs"
                , "repository, it is necessary to set up ssh keys to allow access without"
                , "typing a password.  See the --ssh-export option for help doing this." ]

{-
darcsRev :: SourceTree -> P.RetrieveMethod -> IO (Either SomeException String)
darcsRev tree m =
    try (readProc (shell cmd) >>= return . matchRegex (mkRegex "hash='([^']*)'") . B.unpack . B.concat . keepStdout) >>= 
    return . either Left (maybe (fail $ "could not find hash field in output of '" ++ cmd ++ "'")
                                (\ rev -> Right (show m ++ "=" ++ head rev)))
    where
      cmd = "cd " ++ path ++ " && darcs changes --xml-output"
      path = topdir tree
-}

data DarcsDL
    = DarcsDL
      { _method :: RetrieveMethod
      , _flags :: [P.PackageFlag]
      , _uri :: String
      , _tree :: SourceTree
      , _attr :: String
      } deriving Show

instance T.Download DarcsDL where
    method = _method
    flags = _flags
    getTop = topdir . _tree
    logText m = "Darcs revision: " ++ show (_method m)
    mVersion _ = Nothing
    origTarball _ = Nothing
    flushSource t = do
      base <- sub "darcs"
      liftIO $ removeRecursiveSafely (base </> cksum)
        where
          cksum = show (md5 (B.pack uriAndTag))
          uriAndTag = uriToString id theUri' "" ++ maybe "" (\ tag -> "=" ++ tag) theTag
          theTag = case mapMaybe P.darcsTag (_flags t) of
                     [] -> Nothing
                     [x] -> Just x
                     xs -> error ("Conflicting tags for darcs get of " ++ _uri t ++ ": " ++ show xs)
          theUri' = mustParseURI (_uri t)
    cleanTarget x = \ top -> case any P.isKeepRCS (_flags x) of
                               False -> let cmd = shell ("find " ++ top ++ " -name '_darcs' -maxdepth 1 -prune | xargs rm -rf") in
                                        timeTask (runVE2 [$here] cmd "")
                               True -> return ((Right mempty), 0)
    buildWrapper _ = id
    attrs x = singleton (DarcsChangesId (_attr x))

prepare :: (MonadIO m, MonadCatch m, MonadTop r m, Exception e, MonadError e m) => RetrieveMethod -> [P.PackageFlag] -> String -> m T.SomeDownload
prepare method flags theUri = sub "darcs" >>= prepare' method flags theUri

prepare' :: forall e m. (MonadIO m, MonadCatch m, Exception e, MonadError e m) => RetrieveMethod -> [P.PackageFlag] -> String -> FilePath -> m T.SomeDownload
prepare' method flags theUri base = do
  update >>= maybe recreate (return . Just) >>= finish
    where
      update :: m (Maybe SourceTree)
      update = do
        exists <- liftIO $ doesDirectoryExist dir
        case exists of
          True -> do
            (result :: Either e (ExitCode, String, String)) <- runQE2 [$here] ((proc "darcs" ["whatsnew"]) {cwd = Just dir}) ("" :: String)
            case result of
              Right (ExitFailure 1, "No changes!\n", "") -> do
                do let cmd = (proc "darcs" ["pull", "--all", "--no-allow-conflicts", renderForDarcs theUri']) {cwd = Just dir}
                   result' <- runVE2 [$here] cmd B.empty :: m (Either e (ExitCode, B.ByteString, B.ByteString))
                   case result' of
                     Right (ExitSuccess, _, _) -> liftIO $ Just <$> findSourceTree dir
                     _ -> return Nothing
              _ -> return Nothing -- Not the result expected with a pristine repo
          False -> return Nothing

      recreate :: forall m'. (MonadIO m', MonadCatch m', MonadError e m') => m' (Maybe SourceTree)
      recreate = do
        liftIO $ removeRecursiveSafely dir
        liftIO $ createDirectoryIfMissing True parent
        let cmd = proc "darcs" (["get", renderForDarcs theUri'] ++ maybe [] (\ tag -> [" --tag", tag]) theTag ++ [dir])
        result <- runVE2 [$here] cmd B.empty :: m' (Either e (ExitCode, B.ByteString, B.ByteString))
        case result of
          Right (ExitSuccess, _, _) -> liftIO $ Just <$> findSourceTree dir
          _ -> return Nothing

      finish :: Maybe SourceTree -> m T.SomeDownload
      finish Nothing = error $ "Failure retrieving darcs repo " ++ theUri
      finish (Just tree) = do
          attr <- runQE2 [$here] ((proc "darcs" ["log"]) {cwd = Just dir}) B.empty >>=
                  either (\(e :: e) -> error $ "Failure examining darcs log: " ++ show e) (\ (_, b, _) -> return $ darcsLogChecksum b)
          _ <- liftIO $ fixLink -- this link is just for the convenience of someone poking around in ~/.autobuilder
          return $ T.SomeDownload $ DarcsDL { _method = method
                                            , _flags = flags
                                            , _uri = theUri
                                            , _tree = tree
                                            , _attr = attr }
      -- Maybe we should include the "darcs:" in the string we checksum?
      fixLink :: IO ()
      fixLink = do
          let rm = proc "rm" ["-rf", link]
              ln = proc "ln" ["-s", cksum, link]
          _ <- runV2 [$here] rm B.empty
          _ <- runV2 [$here] ln B.empty
          return ()

      -- Collect all the checksum lines, sort them, and checksum the
      -- result.  The resulting checksum should map one to one with
      -- the state of the darcs repo.
      darcsLogChecksum :: B.ByteString -> String
      darcsLogChecksum = show . md5 . B.unlines . sort . filter (B.isPrefixOf "patch ") . B.lines

      cksum = show (md5 (B.pack uriAndTag))
      name = snd . splitFileName $ (uriPath theUri')
      dir = base </> cksum
      (parent, _) = splitFileName dir
      link = base </> name
      uriAndTag = uriToString id theUri' "" ++ maybe "" (\ tag -> "=" ++ tag) theTag
      theTag = case mapMaybe P.darcsTag flags of
                 [] -> Nothing
                 [x] -> Just x
                 xs -> error ("Conflicting tags for darcs get of " ++ theUri ++ ": " ++ show xs)
      theUri' = mustParseURI theUri


renderForDarcs :: URI -> String
renderForDarcs uri =
    case (uriScheme uri, uriAuthority uri) of
      ("ssh:", Just auth) -> uriUserInfo auth ++ uriRegName auth ++ ":" ++ uriPath uri ++ uriQuery uri ++ uriFragment uri
      ("file:", _) -> uriPath uri
      (_, _) -> show uri

mustParseURI :: String -> URI
mustParseURI s = maybe (error ("Darcs - failed to parse URI: " ++ show s)) id (parseURI s)
