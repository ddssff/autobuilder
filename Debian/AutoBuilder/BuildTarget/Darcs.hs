{-# LANGUAGE OverloadedStrings, Rank2Types, ScopedTypeVariables #-}
module Debian.AutoBuilder.BuildTarget.Darcs
    ( documentation
    , prepare
    ) where

import Control.Monad (when)
import Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Digest.Pure.MD5 (md5)
import Data.Monoid (mempty)
import Data.Set (singleton)
import qualified Debian.AutoBuilder.Types.CacheRec as P
import qualified Debian.AutoBuilder.Types.Download as T
import qualified Debian.AutoBuilder.Types.Packages as P
import qualified Debian.AutoBuilder.Types.ParamRec as P
import Debian.Repo
import qualified Debian.Repo.Fingerprint as P
import Network.URI (URI(..), URIAuth(..), uriToString, parseURI)
import System.Directory
import System.Exit (ExitCode(..))
import System.FilePath
import System.Process (shell, proc, CreateProcess(cwd))
import System.Process.ListLike (readCreateProcess, collectProcessTriple)
import Debian.Repo.Prelude.Verbosity (timeTask)
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

prepare :: (MonadRepos m, MonadTop m) => P.CacheRec -> P.Packages -> String -> m T.Download
prepare cache package theUri =
    do
      base <- sub "darcs"
      let dir = base ++ "/" ++ sum
      liftIO $ when (P.flushSource (P.params cache)) (removeRecursiveSafely dir)
      exists <- liftIO $ doesDirectoryExist dir
      tree <- liftIO $ if exists then verifySource dir else createSource dir
      attr <- liftIO $ readCreateProcess ((proc "darcs" ["changes", "--xml-output"]) {cwd = Just dir}) "" >>= return . show . md5
      _output <- liftIO $ fixLink base
      return $ T.Download { T.package = package
                          , T.getTop = topdir tree
                          , T.logText =  "Darcs revision: " ++ show (P.spec package)
                          , T.mVersion = Nothing
                          , T.origTarball = Nothing
                          , T.cleanTarget =
                              \ top -> case P.keepRCS package of
                                         False -> let cmd = "find " ++ top ++ " -name '_darcs' -maxdepth 1 -prune | xargs rm -rf" in
                                                  timeTask (readProcFailing (shell cmd) "")
                                         True -> return ([], 0)
                          , T.buildWrapper = id
                          , T.attrs = singleton (P.DarcsChangesId attr)
                          }
    where
      verifySource :: FilePath -> IO SourceTree
      verifySource dir =
          -- Note that this logic is the opposite of 'tla changes'
          do (result, _, _) <- readProcFailing (shell ("cd " ++ dir ++ " && darcs whatsnew")) mempty >>= return . collectProcessTriple
             case result of
               ExitSuccess -> removeSource dir >> createSource dir		-- Yes changes
               _ -> updateSource dir				-- No Changes!
      removeSource :: FilePath -> IO ()
      removeSource dir = removeRecursiveSafely dir

      updateSource :: FilePath -> IO SourceTree
      updateSource dir =
          readProcFailing (shell ("cd " ++ dir ++ " && darcs pull --all " ++ renderForDarcs theUri')) "" >>
          -- runTaskAndTest (updateStyle (commandTask ("cd " ++ dir ++ " && darcs pull --all " ++ renderForDarcs theUri))) >>
          findSourceTree dir

      createSource :: FilePath -> IO SourceTree
      createSource dir =
          let (parent, _) = splitFileName dir in
          do createDirectoryIfMissing True parent
             _output <- readProcFailing (shell cmd) ""
             findSourceTree dir
          where
            cmd = unwords $ ["darcs", "get", renderForDarcs theUri'] ++ maybe [] (\ tag -> [" --tag", "'" ++ tag ++ "'"]) theTag ++ [dir]
      -- Maybe we should include the "darcs:" in the string we checksum?
      fixLink base =
          let link = base ++ "/" ++ name
              cmd = "rm -rf " ++ link ++ " && ln -s " ++ sum ++ " " ++ link in
          readProcFailing (shell cmd) ""
      name = snd . splitFileName $ (uriPath theUri')
      sum = show (md5 (B.pack uriAndTag))
      uriAndTag = uriToString id theUri' "" ++ maybe "" (\ tag -> "=" ++ tag) theTag
      theTag = case P.testPackageFlag (\ x -> case x of P.DarcsTag s -> Just s; _ -> Nothing) package of
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
