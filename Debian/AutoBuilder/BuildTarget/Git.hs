{-# LANGUAGE GADTs, OverloadedStrings, Rank2Types, ScopedTypeVariables #-}
module Debian.AutoBuilder.BuildTarget.Git where

import Control.Exception (try, SomeException)
import Control.Monad.Error (ErrorT)
import Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Digest.Pure.MD5 (md5)
import Data.Maybe (mapMaybe)
import Data.Monoid (mempty)
import Data.Set (singleton)
import qualified Debian.AutoBuilder.Types.Download as T
import qualified Debian.AutoBuilder.Types.Packages as P
import Debian.Repo (SourceTree, topdir, MonadRepos, MonadTop, sub, findSourceTree)
import Debian.Repo.Fingerprint (RetrieveMethod, RetrieveAttribute(GitCommit), GitSpec(Branch, Commit))
import Debian.Repo.Prelude.Process (readProcessVE, readProcessV, timeTask)
import Network.URI (URI(..), URIAuth(..), uriToString, parseURI)
import System.Directory
import System.Exit (ExitCode(..))
import System.FilePath
import System.Process (proc, shell, CreateProcess(cwd, cmdspec))
import System.Process.ListLike (readCreateProcessWithExitCode, showCmdSpecForUser, showCreateProcessForUser)
import System.Unix.Directory
import Text.Regex

documentation :: [String]
documentation = [ "darcs:<string> - a target of this form obtains the source code by running"
                , "darcs get <string>.  If the argument needs to use ssh to reach the darcs"
                , "repository, it is necessary to set up ssh keys to allow access without"
                , "typing a password.  See the --ssh-export option for help doing this." ]

darcsRev :: SourceTree -> RetrieveMethod -> IO (Either SomeException String)
darcsRev tree m =
    try (readProcessV (cmd {cwd = Just path}) mempty >>= (\ (_, out, _) -> return out) >>=
         return . matchRegex (mkRegex "hash='([^']*)'") . B.unpack) >>=
    return . either Left (maybe (fail $ "could not find hash field in output of '" ++ showCmdSpecForUser (cmdspec cmd) ++ "'")
                                (\ rev -> Right (show m ++ "=" ++ head rev)))
    where
      cmd = proc "darcs" ["changes", "--xml-output"]
      path = topdir tree

showCmd = showCmdSpecForUser

data GitDL
    = GitDL
      { method :: RetrieveMethod
      , flags :: [P.PackageFlag]
      , uri :: String
      , gitspecs :: [GitSpec]
      , tree :: SourceTree
      , latestCommit :: String
      }

instance T.Download GitDL where
    method = method
    flags = flags
    getTop = topdir . tree
    logText x = "git revision: " ++ show (method x)
    flushSource x =
        let theUri' = mustParseURI (uri x)
            theBranch = case mapMaybe P.gitBranch (flags x) of
                          [] -> Nothing
                          [x] -> Just x
                          xs -> error ("Conflicting branches for git clone of " ++ uri x ++ ": " ++ show xs)
            uriAndBranch = uriToString id theUri' "" ++ maybe "" (\ branch -> "=" ++ branch) theBranch
            sum = show (md5 (B.pack uriAndBranch)) in
        sub ("git" </> sum) >>= liftIO . removeRecursiveSafely
    cleanTarget x =
        (\ top -> case any P.isKeepRCS (flags x) of
                    False -> let cmd = "find " ++ top ++ " -name '.git' -maxdepth 1 -prune | xargs rm -rf" in timeTask (readProcessVE (shell cmd) "")
                    True -> return (Right mempty, 0))
    attrs x = singleton $ GitCommit $ latestCommit x

prepare :: (MonadRepos m, MonadTop m) => RetrieveMethod -> [P.PackageFlag] -> String -> [GitSpec] -> m T.SomeDownload
prepare method flags theUri gitspecs =
    sub "git" >>= \ base ->
    sub ("git" </> sum) >>= \ dir -> liftIO $
    do
      tree <- prepareSource dir
      _output <- fixLink base
      let p = (proc "git" ["log", "-n", "1", "--pretty=%H"]) {cwd = Just dir}
      (code, out, _) <- readCreateProcessWithExitCode p ""
      commit <- case code of
                  ExitSuccess -> return . head . lines $ out
                  _ -> error $ showCreateProcessForUser p ++ " -> " ++ show code
      return $ T.SomeDownload $ GitDL { method = method
                                      , flags = flags
                                      , uri = theUri
                                      , gitspecs = gitspecs
                                      , tree = tree
                                      , latestCommit = commit }
    where

      prepareSource dir =
          do result1 <- pullSource dir
             case result1 of
               True -> findSourceTree dir
               False ->
                   do removeSource dir
                      result3 <- cloneSource dir >>= \ result2 -> if result2 then pullSource dir else return False
                      if result3 then findSourceTree dir else error ("Failure preparing " ++ show dir)

      test1 (Right (ExitSuccess, _, _)) = return True
      test1 _ = return False

      pullSource dir =
          let commit = mapMaybe (\ x -> case x of (Commit s) -> Just s; _ -> Nothing) gitspecs in
          do exists <- doesDirectoryExist dir
             case (exists, commit) of
               (False, _) -> return False
               (True, []) -> readProcessVE ((proc "git" ["pull", "--all"]) {cwd = Just dir}) B.empty >>= test1
               (True, [commit]) -> readProcessVE ((proc "git" (["reset", "--hard", commit])) {cwd = Just dir}) B.empty >>= test1
               (True, commits) -> error $ "Multiple commit arguments for " ++ show method ++ ": " ++ show commits

      cloneSource dir =
          do let (parent, _) = splitFileName dir
             createDirectoryIfMissing True parent
             removeRecursiveSafely dir
             let clone = proc "git" (["clone", renderForGit theUri'] ++ concat (map (\ x -> case x of (Branch s) -> ["--branch", s]; _ -> []) gitspecs) ++ [dir])
             readProcessVE clone B.empty >>= test1
{-
      testSource dir =
          do result <- readProcessVE ((proc "git" ["status", "--porcelain"]) {cwd = Just dir}) mempty
             case result of
               Right (ExitSuccess, out, _) | B.null out -> return True
               _ -> return False
-}
      removeSource :: FilePath -> IO ()
      removeSource dir = removeRecursiveSafely dir

      fixLink base =
          let link = base </> name in
          readProcessV (proc "rm" ["-rf", link]) B.empty >>
          readProcessV (proc "ln" ["-s", sum, link]) B.empty
      name = snd . splitFileName $ (uriPath theUri')
      sum = show (md5 (B.pack uriAndBranch))
      -- Maybe we should include the "git:" in the string we checksum?  -- DSF
      -- Need more info to answer that, but addition of git makes it more likely. -- CB
      uriAndBranch = uriToString id theUri' "" ++ maybe "" (\ branch -> "=" ++ branch) theBranch
      theBranch = case mapMaybe P.gitBranch flags of
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
