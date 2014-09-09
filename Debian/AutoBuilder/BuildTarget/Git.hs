{-# LANGUAGE OverloadedStrings, Rank2Types, ScopedTypeVariables #-}
module Debian.AutoBuilder.BuildTarget.Git where

import Control.Exception (try, SomeException)
import Control.Monad
import Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Digest.Pure.MD5 (md5)
import Data.Maybe (mapMaybe)
import Data.Monoid (mempty)
import Data.Set (singleton)
import qualified Debian.AutoBuilder.Types.CacheRec as P
import qualified Debian.AutoBuilder.Types.Download as T
import qualified Debian.AutoBuilder.Types.Packages as P
import qualified Debian.AutoBuilder.Types.ParamRec as P
import Debian.Repo
import qualified Debian.Repo.Fingerprint as P
import Debian.Repo.Prelude.Verbosity (timeTask)
import Network.URI (URI(..), URIAuth(..), uriToString, parseURI)
import System.Directory
import System.Exit (ExitCode(..))
import System.FilePath
import System.Process (proc, shell, CreateProcess(cwd, cmdspec))
import System.Process.String (readCreateProcessWithExitCode, collectProcessTriple, collectProcessOutput', showCmdSpecForUser, displayCreateProcess)
import System.Unix.Directory
import Text.Regex

documentation :: [String]
documentation = [ "darcs:<string> - a target of this form obtains the source code by running"
                , "darcs get <string>.  If the argument needs to use ssh to reach the darcs"
                , "repository, it is necessary to set up ssh keys to allow access without"
                , "typing a password.  See the --ssh-export option for help doing this." ]

darcsRev :: SourceTree -> P.RetrieveMethod -> IO (Either SomeException String)
darcsRev tree m =
    try (readProc (cmd {cwd = Just path}) mempty >>= collectProcessOutput' cmd >>=
         return . matchRegex (mkRegex "hash='([^']*)'") . B.unpack) >>=
    return . either Left (maybe (fail $ "could not find hash field in output of '" ++ showCmdSpecForUser (cmdspec cmd) ++ "'")
                                (\ rev -> Right (show m ++ "=" ++ head rev)))
    where
      cmd = proc "darcs" ["changes", "--xml-output"]
      path = topdir tree

showCmd = showCmdSpecForUser

prepare :: (MonadRepos m, MonadTop m) => P.CacheRec -> P.Packages -> String -> [P.GitSpec] -> m T.Download
prepare cache package theUri gitspecs =
    sub "git" >>= \ base ->
    sub ("git" </> sum) >>= \ dir -> liftIO $
    do
      when (P.flushSource (P.params cache)) (removeRecursiveSafely dir)
      exists <- doesDirectoryExist dir
      tree <- if exists then verifySource dir else createSource dir
      _output <- fixLink base
      let p = (proc "git" ["log", "-n", "1", "--pretty=%H"]) {cwd = Just dir}
      (code, out, _) <- readCreateProcessWithExitCode p ""
      commit <- case code of
                  ExitSuccess -> return . head . lines $ out
                  _ -> error $ displayCreateProcess p ++ " -> " ++ show code
      return $ T.Download { T.package = package
                          , T.getTop = topdir tree
                          , T.logText =  "git revision: " ++ show (P.spec package)
                          , T.mVersion = Nothing
                          , T.origTarball = Nothing
                          , T.cleanTarget =
                              \ top -> case P.keepRCS package  of
                                         False -> let cmd = "find " ++ top ++ " -name '.git' -maxdepth 1 -prune | xargs rm -rf" in timeTask (readProc (shell cmd) "")
                                         True -> return ([], 0)
                          , T.buildWrapper = id
                          , T.attrs = singleton (P.GitCommit commit)
                          }
    where
      verifySource :: FilePath -> IO SourceTree
      verifySource dir =
          -- Note that this logic is the opposite of 'tla changes'
          do (result, out, _) <- readProc ((proc "git" ["status", "--porcelain"]) {cwd = Just dir}) mempty >>= return . collectProcessTriple

      -- CB  No output lines means no changes
      -- CB  git reset --hard    will remove all edits back to the most recent commit

      -- The status code does not reflect whether changes were made
             case (result, B.null out) of
               (ExitSuccess, False) -> removeSource dir >> createSource dir		-- Yes changes
               (ExitSuccess, True) -> updateSource dir					-- No Changes!
               _ -> error $ "git failure"
      removeSource :: FilePath -> IO ()
      removeSource dir = removeRecursiveSafely dir

      updateSource :: FilePath -> IO SourceTree
      updateSource dir =
          readProc ((proc "git" ["pull", "--all"]) {cwd = Just dir}) "" >>
          -- runTaskAndTest (updateStyle (commandTask ("cd " ++ dir ++ " && darcs pull --all " ++ renderForDarcs theUri))) >>
          findSourceTree dir

      createSource :: FilePath -> IO SourceTree
      createSource dir =
          do let (parent, _) = splitFileName dir
             createDirectoryIfMissing True parent
             removeRecursiveSafely dir
             let p = proc "git" (["clone", renderForGit theUri'] ++ concat (map (\ x -> case x of (P.Branch s) -> ["--branch", s]; _ -> []) gitspecs) ++ [dir])
             (code, _, _) <- readProc p "" >>= return . collectProcessTriple
             case (code, mapMaybe (\ x -> case x of (P.Commit s) -> Just s; _ -> Nothing) gitspecs) of
                    (ExitFailure _, _) -> error $ "Git failed: " ++ displayCreateProcess p ++ " -> " ++ show code
                    (_, []) -> return ()
                    (_, [commit]) -> do
                      (code2, _, _) <- readProc ((proc "git" ["reset", "--hard", commit]) {cwd = Just dir}) "" >>= return . collectProcessTriple
                      case code2 of
                        ExitFailure _ -> error "git reset failed"
                        ExitSuccess -> return ()
                    (_, commits) -> error $ "Git target specifies multiple commits: " ++ show commits
             findSourceTree dir

      -- CB  git reset --hard    will remove all edits back to the most recent commit
      fixLink base =
          let link = base </> name in
          readProc (proc "rm" ["-rf", link]) "" >>
          readProc (proc "ln" ["-s", sum, link]) ""
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
mustParseURI s = maybe (error ("Git - failed to parse URI: " ++ show s)) id (parseURI s)
