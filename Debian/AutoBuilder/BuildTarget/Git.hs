{-# LANGUAGE OverloadedStrings, Rank2Types, ScopedTypeVariables #-}
module Debian.AutoBuilder.BuildTarget.Git where

import Control.Exception (try, SomeException)
import Control.Monad
import Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Digest.Pure.MD5 (md5)
import Data.Maybe (mapMaybe)
import Data.Set (singleton)
import qualified Debian.AutoBuilder.Types.CacheRec as P
import qualified Debian.AutoBuilder.Types.Download as T
import qualified Debian.AutoBuilder.Types.Packages as P
import qualified Debian.AutoBuilder.Types.ParamRec as P
import Debian.Repo
import Network.URI (URI(..), URIAuth(..), uriToString, parseURI)
import System.Directory
import System.Exit (ExitCode(..))
import System.FilePath
import System.Process (proc, shell, CmdSpec(..), CreateProcess(cwd, cmdspec), showCommandForUser)
import System.Process.Progress (keepStdout, keepResult, timeTask)
import System.Process.Read (readCreateProcess)
import System.Unix.Directory
import Text.Regex

documentation :: [String]
documentation = [ "darcs:<string> - a target of this form obtains the source code by running"
                , "darcs get <string>.  If the argument needs to use ssh to reach the darcs"
                , "repository, it is necessary to set up ssh keys to allow access without"
                , "typing a password.  See the --ssh-export option for help doing this." ]

darcsRev :: SourceTree -> P.RetrieveMethod -> IO (Either SomeException String)
darcsRev tree m =
    try (readProc (cmd {cwd = Just path}) >>=
         return . matchRegex (mkRegex "hash='([^']*)'") . B.unpack . B.concat . keepStdout) >>= 
    return . either Left (maybe (fail $ "could not find hash field in output of '" ++ showCmd (cmdspec cmd) ++ "'")
                                (\ rev -> Right (show m ++ "=" ++ head rev)))
    where
      cmd = proc "darcs" ["changes", "--xml-output"]
      path = topdir tree

showCmd (RawCommand cmd args) = showCommandForUser cmd args
showCmd (ShellCommand cmd) = cmd

prepare :: (MonadRepos m, MonadTop m) => P.CacheRec -> P.Packages -> String -> [P.GitSpec] -> m T.Download
prepare cache package theUri gitspecs =
    sub "git" >>= \ base ->
    sub ("git" </> sum) >>= \ dir -> liftIO $
    do
      when (P.flushSource (P.params cache)) (removeRecursiveSafely dir)
      exists <- doesDirectoryExist dir
      tree <- if exists then verifySource dir else createSource dir
      _output <- fixLink base
      commit <- readCreateProcess ((proc "git" ["log", "-n", "1", "--pretty=%H"]) {cwd = Just dir}) "" >>= return . head . lines
      return $ T.Download { T.package = package
                          , T.getTop = topdir tree
                          , T.logText =  "git revision: " ++ show (P.spec package)
                          , T.mVersion = Nothing
                          , T.origTarball = Nothing
                          , T.cleanTarget =
                              \ top -> case P.keepRCS package  of
                                         False -> let cmd = "find " ++ top ++ " -name '.git' -maxdepth 1 -prune | xargs rm -rf" in timeTask (runProc (shell cmd))
                                         True -> return ([], 0)
                          , T.buildWrapper = id
                          , T.attrs = singleton (P.GitCommit commit)
                          }
    where
      verifySource :: FilePath -> IO SourceTree
      verifySource dir =
          -- Note that this logic is the opposite of 'tla changes'
          do result <- readProc ((proc "git" ["status", "--porcelain"]) {cwd = Just dir}) >>= return . keepResult

      -- CB  No output lines means no changes
      -- CB  git reset --hard    will remove all edits back to the most recent commit

      -- The status code does not reflect whether changes were made
             case result of
               (ExitSuccess : _) -> removeSource dir >> createSource dir		-- Yes changes
               _ -> updateSource dir				-- No Changes!
      removeSource :: FilePath -> IO ()
      removeSource dir = removeRecursiveSafely dir

      updateSource :: FilePath -> IO SourceTree
      updateSource dir =
          runProc ((proc "git" ["pull", "--all", "--commit", renderForGit theUri']) {cwd = Just dir}) >>
          -- runTaskAndTest (updateStyle (commandTask ("cd " ++ dir ++ " && darcs pull --all " ++ renderForDarcs theUri))) >>
          findSourceTree dir

      createSource :: FilePath -> IO SourceTree
      createSource dir =
          let (parent, _) = splitFileName dir in
          do createDirectoryIfMissing True parent
             _ <- runProc (proc "git" (["clone", renderForGit theUri'] ++ concat (map (\ x -> case x of (P.Branch s) -> ["--branch", s]; _ -> []) gitspecs) ++ [dir]))
             _ <- case mapMaybe (\ x -> case x of (P.Commit s) -> Just s; _ -> Nothing) gitspecs of
                    [] -> return []
                    [commit] -> runProc ((proc "git" ["reset", "--hard", commit]) {cwd = Just dir})
                    commits -> error $ "Git target specifies multiple commits: " ++ show commits
             findSourceTree dir

      -- CB  git reset --hard    will remove all edits back to the most recent commit
      fixLink base =
          let link = base </> name in
          runProc (proc "rm" ["-rf", link]) >>
          runProc (proc "ln" ["-s", sum, link])
      name = snd . splitFileName $ (uriPath theUri')
      sum = show (md5 (B.pack uriAndBranch))
      -- Maybe we should include the "git:" in the string we checksum?  -- DSF
      -- Need more info to answer that, but addition of git makes it more likely. -- CB
      uriAndBranch = uriToString id theUri' "" ++ maybe "" (\ branch -> "=" ++ branch) theBranch
      theBranch = case P.testPackageFlag (\ x -> case x of P.GitBranch s -> Just s; _ -> Nothing) package of
                    [] -> Nothing
                    [x] -> Just x
                    xs -> error ("Conflicting branches for git clone of " ++ theUri ++ ": " ++ show xs)
      theUri' = mustParseURI theUri

renderForGit :: URI -> String
renderForGit uri =
    case (uriScheme uri, uriAuthority uri) of
      ("ssh:", Just auth) -> uriUserInfo auth ++ uriRegName auth ++ ":" ++ uriPath uri ++ uriQuery uri ++ uriFragment uri
      (_, _) -> show uri

mustParseURI :: String -> URI
mustParseURI s = maybe (error ("Darcs - failed to parse URI: " ++ show s)) id (parseURI s)
