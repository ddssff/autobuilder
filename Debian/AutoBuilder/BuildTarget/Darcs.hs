{-# LANGUAGE CPP, GADTs, OverloadedStrings, Rank2Types, ScopedTypeVariables #-}
module Debian.AutoBuilder.BuildTarget.Darcs
    ( documentation
    , prepare
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Digest.Pure.MD5 (md5)
import Data.List (sort)
import Data.Maybe (mapMaybe)
#if !MIN_VERSION_base(4,8,0)
import Data.Monoid (mempty)
#endif
import Data.Set (singleton)
import qualified Debian.AutoBuilder.Types.Download as T
import qualified Debian.AutoBuilder.Types.Packages as P
import Debian.Repo
import Debian.Repo.Fingerprint (RetrieveMethod, RetrieveAttribute(DarcsChangesId))
import Debian.Repo.Prelude.Process (runQE, runVE, runV)
import Network.URI (URI(..), URIAuth(..), uriToString, parseURI)
import System.Directory
import System.Exit (ExitCode(..))
import System.FilePath
import System.Process (shell, proc, CreateProcess(cwd))
import Debian.Repo.Prelude.Process (timeTask)
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
      { method :: RetrieveMethod
      , flags :: [P.PackageFlag]
      , uri :: String
      , tree :: SourceTree
      , attr :: String
      } deriving Show

instance T.Download DarcsDL where
    method = method
    flags = flags
    getTop = topdir . tree
    logText x = "Darcs revision: " ++ show (method x)
    mVersion _ = Nothing
    origTarball _ = Nothing
    flushSource x = do
      base <- sub "darcs"
      liftIO $ removeRecursiveSafely (base </> sum)
        where
          sum = show (md5 (B.pack uriAndTag))
          uriAndTag = uriToString id theUri' "" ++ maybe "" (\ tag -> "=" ++ tag) theTag
          theTag = case mapMaybe P.darcsTag (flags x) of
                     [] -> Nothing
                     [x] -> Just x
                     xs -> error ("Conflicting tags for darcs get of " ++ uri x ++ ": " ++ show xs)
          theUri' = mustParseURI (uri x)
    cleanTarget x = \ top -> case any P.isKeepRCS (flags x) of
                               False -> let cmd = shell ("find " ++ top ++ " -name '_darcs' -maxdepth 1 -prune | xargs rm -rf") in
                                        timeTask (runVE cmd "")
                               True -> return ((Right mempty), 0)
    buildWrapper _ = id
    attrs x = singleton (DarcsChangesId (attr x))

prepare :: (MonadRepos s m, MonadTop r m) => RetrieveMethod -> [P.PackageFlag] -> String -> m T.SomeDownload
prepare method flags theUri = sub "darcs" >>= prepare' method flags theUri

prepare' :: forall s m. MonadRepos s m => RetrieveMethod -> [P.PackageFlag] -> String -> FilePath -> m T.SomeDownload
prepare' method flags theUri base = do
  update >>= maybe recreate (return . Just) >>= finish
    where
      update :: m (Maybe SourceTree)
      update = do
        exists <- liftIO $ doesDirectoryExist dir
        case exists of
          True -> do
            result <- runQE ((proc "darcs" ["whatsnew"]) {cwd = Just dir}) ("" :: String)
            case result of
              Right (ExitFailure 1, "No changes!\n", "") -> do
                do let cmd = (proc "darcs" ["pull", "--all", "--no-allow-conflicts", renderForDarcs theUri']) {cwd = Just dir}
                   result <- runVE cmd B.empty
                   case result of
                     Right (ExitSuccess, _, _) -> liftIO $ Just <$> findSourceTree dir
                     _ -> return Nothing
              _ -> return Nothing -- Not the result expected with a pristine repo
          False -> return Nothing

      recreate :: m (Maybe SourceTree)
      recreate = do
        liftIO $ removeRecursiveSafely dir
        liftIO $ createDirectoryIfMissing True parent
        let cmd = proc "darcs" (["get", renderForDarcs theUri'] ++ maybe [] (\ tag -> [" --tag", tag]) theTag ++ [dir])
        result <- runVE cmd B.empty
        case result of
          Right (ExitSuccess, _, _) -> liftIO $ Just <$> findSourceTree dir
          _ -> return Nothing

      finish :: Maybe SourceTree -> m T.SomeDownload
      finish Nothing = error $ "Failure retrieving darcs repo " ++ theUri
      finish (Just tree) = do
          attr <- liftIO (runQE ((proc "darcs" ["log"]) {cwd = Just dir}) B.empty) >>=
                  either (\ e -> error $ "Failure examining darcs log: " ++ show e) (\ (_, b, _) -> return $ darcsLogChecksum b)
          _ <- liftIO $ fixLink -- this link is just for the convenience of someone poking around in ~/.autobuilder
          return $ T.SomeDownload $ DarcsDL { method = method
                                            , flags = flags
                                            , uri = theUri
                                            , tree = tree
                                            , attr = attr }
      -- Maybe we should include the "darcs:" in the string we checksum?
      fixLink :: IO ()
      fixLink = do
          let rm = proc "rm" ["-rf", link]
              ln = proc "ln" ["-s", sum, link]
          _ <- runV rm B.empty
          _ <- runV ln B.empty
          return ()

      -- Collect all the checksum lines, sort them, and checksum the
      -- result.  The resulting checksum should map one to one with
      -- the state of the darcs repo.
      darcsLogChecksum :: B.ByteString -> String
      darcsLogChecksum = show . md5 . B.unlines . sort . filter (B.isPrefixOf "patch ") . B.lines

      sum = show (md5 (B.pack uriAndTag))
      name = snd . splitFileName $ (uriPath theUri')
      dir = base </> sum
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
