{-# LANGUAGE GADTs, OverloadedStrings, Rank2Types, ScopedTypeVariables #-}
module Debian.AutoBuilder.BuildTarget.Darcs
    ( documentation
    , prepare
    ) where

import Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Digest.Pure.MD5 (md5)
import Data.List (sort)
import Data.Maybe (mapMaybe)
import Data.Monoid (mempty)
import Data.Set (singleton)
import qualified Debian.AutoBuilder.Types.Download as T
import qualified Debian.AutoBuilder.Types.Packages as P
import Debian.Repo
import Debian.Repo.Fingerprint (RetrieveMethod, RetrieveAttribute(DarcsChangesId))
import Debian.Repo.Prelude.Process (readProcessE, readProcessV)
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
      }

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
                                        timeTask (readProcessE cmd "")
                               True -> return ((Right mempty), 0)
    buildWrapper _ = id
    attrs x = singleton (DarcsChangesId (attr x))

prepare :: (MonadRepos m, MonadTop m) =>
           RetrieveMethod -> [P.PackageFlag] -> String -> m T.SomeDownload
prepare method flags theUri =
    do
      base <- sub "darcs"
      let dir = base ++ "/" ++ sum
      exists <- liftIO $ doesDirectoryExist dir
      tree <- liftIO $ if exists then verifySource dir else createSource dir
      -- Filter out the patch hash values, sort them because darcs
      -- patches don't have a total ordering, and then return the
      -- checksum.
      attr <- liftIO $ readProcessV ((proc "darcs" ["log"]) {cwd = Just dir}) B.empty >>= \ (_, b, _) -> return $ darcsLogChecksum b
      _output <- liftIO $ fixLink base
      return $ T.SomeDownload $ DarcsDL { method = method
                                        , flags = flags
                                        , uri = theUri
                                        , tree = tree
                                        , attr = attr }
    where
      -- Collect all the checksum lines, sort them, and checksum the
      -- result.  The resulting checksum should map one to one with
      -- the state of the darcs repo.
      darcsLogChecksum :: B.ByteString -> String
      darcsLogChecksum = show . md5 . B.unlines . sort . filter (B.isPrefixOf "patch ") . B.lines
      verifySource :: FilePath -> IO SourceTree
      verifySource dir =
          -- Note that this logic is the opposite of 'tla changes'
          do result <- readProcessE ((proc "darcs" ["whatsnew"]) {cwd = Just dir}) ("" :: String)
             case result of
               Right (ExitSuccess, _, _) -> removeSource dir >> createSource dir		-- Yes changes
               _ -> updateSource dir				-- No Changes!
      removeSource :: FilePath -> IO ()
      removeSource dir = removeRecursiveSafely dir

      updateSource :: FilePath -> IO SourceTree
      updateSource dir = do
          let cmd = (proc "darcs" ["pull", "--all", "--no-allow-conflicts", renderForDarcs theUri']) {cwd = Just dir}
          _ <- readProcessV cmd B.empty
          -- runTaskAndTest (updateStyle (commandTask ("cd " ++ dir ++ " && darcs pull --all " ++ renderForDarcs theUri))) >>
          findSourceTree dir

      createSource :: FilePath -> IO SourceTree
      createSource dir =
          let (parent, _) = splitFileName dir in
          do createDirectoryIfMissing True parent
             _output <- readProcessV cmd B.empty
             findSourceTree dir
          where
            cmd = proc "darcs" (["get", renderForDarcs theUri'] ++ maybe [] (\ tag -> [" --tag", tag]) theTag ++ [dir])
      -- Maybe we should include the "darcs:" in the string we checksum?
      fixLink base = do
          let link = base ++ "/" ++ name
              rm = proc "rm" ["-rf", link]
              ln = proc "ln" ["-s", sum, link]
          _ <- readProcessV rm B.empty
          readProcessV ln B.empty
      name = snd . splitFileName $ (uriPath theUri')
      sum = show (md5 (B.pack uriAndTag))
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
