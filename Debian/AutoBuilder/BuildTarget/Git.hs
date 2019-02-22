{-# LANGUAGE CPP, FlexibleContexts, GADTs, OverloadedStrings, PackageImports, Rank2Types, ScopedTypeVariables, TemplateHaskell #-}
module Debian.AutoBuilder.BuildTarget.Git where

import Control.Exception (Exception, SomeException, try)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Except (MonadError, MonadIO)
import Control.Monad.Reader (MonadReader)
--import Control.Monad.State (MonadState)
import Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Digest.Pure.MD5 (md5)
import Data.List (sort)
--import Data.Maybe (listToMaybe, mapMaybe)
import Data.Set (singleton)
import qualified Debian.AutoBuilder.Types.Download as T
import qualified Debian.AutoBuilder.Types.Packages as P
import Debian.Repo (SourceTree, topdir, sub, findSourceTree)
import Debian.Repo.Fingerprint (RetrieveMethod, RetrieveAttribute(GitCommit), GitSpec(Branch, Commit))
import Debian.Repo.SourceTree (HasSourceTree)
import Debian.Repo.Top (HasTop)
import Debian.TH (here)
import Extra.Process (runVE2, runV2, timeTask)
import Network.URI (URI(..), URIAuth(..), uriToString, parseURI)
import System.Directory
import System.Exit (ExitCode(..))
import System.FilePath
import System.Process (proc, shell, CreateProcess(cwd, cmdspec))
import System.Process.ListLike (CmdSpec, readCreateProcessWithExitCode, showCmdSpecForUser, showCreateProcessForUser)
import System.Unix.Directory
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), prettyShow, text)
import "regex-compat-tdfa" Text.Regex

instance Pretty CreateProcess where
    pPrint = text . showCreateProcessForUser

documentation :: [String]
documentation = [ "darcs:<string> - a target of this form obtains the source code by running"
                , "darcs get <string>.  If the argument needs to use ssh to reach the darcs"
                , "repository, it is necessary to set up ssh keys to allow access without"
                , "typing a password.  See the --ssh-export option for help doing this." ]

darcsRev :: SourceTree -> RetrieveMethod -> IO (Either SomeException String)
darcsRev tree m =
    try (runV2 [$here] (cmd {cwd = Just path}) mempty >>= (\ (_, out, _) -> return out) >>=
         return . matchRegex (mkRegex "hash='([^']*)'") . B.unpack) >>=
    return . either Left (maybe (fail $ "could not find hash field in output of '" ++ showCmdSpecForUser (cmdspec cmd) ++ "'")
                                (\ rev -> Right (show m ++ "=" ++ head rev)))
    where
      cmd = proc "darcs" ["changes", "--xml-output"]
      path = topdir tree

showCmd :: CmdSpec -> String
showCmd = showCmdSpecForUser

data GitDL
    = GitDL
      { _gitMethod :: RetrieveMethod
      , _gitFlags :: [P.PackageFlag]
      , _gitUri :: String
      , _gitSpecs :: [GitSpec]
      , _gitTree :: SourceTree
      , _gitLatestCommit :: String
      } deriving Show

instance T.Download GitDL where
    method = _gitMethod
    flags = _gitFlags
    getTop = topdir . _gitTree
    logText x = "git revision: " ++ show (_gitMethod x)
    flushSource x = sub ("git" </> gitSum (_gitFlags x) (_gitUri x) (_gitSpecs x)) >>= liftIO . removeRecursiveSafely
    cleanTarget x =
        (\ top -> case any P.isKeepRCS (_gitFlags x) of
                    False -> let cmd = "find " ++ top ++ " -name '.git' -maxdepth 1 -prune | xargs rm -rf" in timeTask (runVE2 [$here] (shell cmd) "")
                    True -> return (Right mempty, 0))
    attrs x = singleton $ GitCommit $ _gitLatestCommit x

-- Maybe we should include the "git:" in the string we checksum?  -- DSF
-- Need more info to answer that, but addition of git makes it more likely. -- CB
-- We need all values in the [GitSpec] field
gitSum :: [P.PackageFlag] -> String -> [GitSpec] -> String
gitSum flags theUri gitspecs = show (md5 (B.pack uriAndBranch))
    where
      uriAndBranch = uriToString id (mustParseURI theUri) "" ++ branchAndCommitSuffix
      branchAndCommitSuffix = case sort gitspecs of
                                [] -> ""
                                _ -> "=" ++ show (sort gitspecs)

prepare :: forall r e m. (MonadIO m, MonadCatch m, HasTop r, MonadReader r m, Exception e, MonadError e m, HasSourceTree SourceTree m) => RetrieveMethod -> [P.PackageFlag] -> String -> [GitSpec] -> m T.SomeDownload
prepare method flags theUri gitspecs = do
  base <- sub "git"
  dir <- sub ("git" </> gitSum flags theUri gitspecs)
  tree <- prepareSource dir
  _output <- fixLink base
  let p = (proc "git" ["log", "-n", "1", "--pretty=%H"]) {cwd = Just dir}
  liftIO $ putStrLn (" -> " ++ showCreateProcessForUser p)
  (code, out, _) <- liftIO $ readCreateProcessWithExitCode p ""
  commit <- case code of
              ExitSuccess -> return . head . lines $ out
              _ -> error $ showCreateProcessForUser p ++ " -> " ++ show code
  let x = GitDL { _gitMethod = method
                , _gitFlags = flags
                , _gitUri = theUri
                , _gitSpecs = gitspecs
                , _gitTree = tree
                , _gitLatestCommit = commit }
  return $ T.SomeDownload x
    where
      prepareSource :: FilePath -> m SourceTree
      prepareSource dir =
          do result1 <- pullSource dir
             case result1 of
               True -> findSourceTree dir
               False ->
                   do liftIO $ removeSource dir
                      result3 <- cloneSource dir >>= \ result2 -> if result2 then pullSource dir else return False
                      if result3 then findSourceTree dir else error ("Failure preparing " ++ show dir)

      test1 :: Either e (ExitCode, B.ByteString, B.ByteString) -> m Bool
      test1 (Right (ExitSuccess, _, _)) = return True
      test1 _ = return False

      pullSource :: forall m'. (MonadIO m', MonadCatch m', MonadError e m') => FilePath -> m' Bool
      pullSource dir =
          let commit = foldr (\x r -> case x of Commit s -> Just s; _ -> r) Nothing gitspecs
              branch = foldr (\x r -> case x of Branch s -> s; _ -> r) "master" gitspecs in
          do exists <- liftIO $ doesDirectoryExist dir
             case exists of
               False -> return False
               True -> do
                 readProc (proc "git" ["reset", "--hard"]) {cwd = Just dir} B.empty -- Get rid of any local changes
                 readProc (proc "git" ["pull", "--all"]) {cwd = Just dir} B.empty -- Get all remote changes
                 readProc (proc "git" ["checkout", "-f", branch]) {cwd = Just dir} B.empty -- Switch to correct branch
                 maybe (return ()) (\x -> readProc (proc "git" ["reset", "--hard", x]) {cwd = Just dir} B.empty) commit -- Go to desired commit
                 return True
      cloneSource :: FilePath -> m Bool
      cloneSource dir =
          do let (parent, _) = splitFileName dir
             liftIO $ createDirectoryIfMissing True parent
             liftIO $ removeRecursiveSafely dir
             let clone = proc "git" (["clone", renderForGit theUri'] ++ concat (map (\ x -> case x of (Branch s) -> ["--branch", s]; _ -> []) gitspecs) ++ [dir])
             runVE2 [$here] clone B.empty >>= test1

      readProc pr inp = runV2 [$here] pr inp >>= testCode pr
      testCode _pr (ExitSuccess, _, _) = return ()
      testCode pr (ExitFailure s, out, err) = error (prettyShow pr ++ " -> " ++ show s ++ "\n  out: " ++ show out ++ "\n  err: " ++ show err)

{-
      testSource dir =
          do result <- runVE2 ((proc "git" ["status", "--porcelain"]) {cwd = Just dir}) mempty
             case result of
               Right (ExitSuccess, out, _) | B.null out -> return True
               _ -> return False
-}
      removeSource :: FilePath -> IO ()
      removeSource dir = removeRecursiveSafely dir

      fixLink base =
          let link = base </> name in
          runV2 [$here] (proc "rm" ["-rf", link]) B.empty >>
          runV2 [$here] (proc "ln" ["-s", gitSum flags theUri gitspecs, link]) B.empty
      name = snd . splitFileName $ (uriPath theUri')

      theUri' = mustParseURI theUri

renderForGit :: URI -> String
renderForGit uri =
    case (uriScheme uri, uriAuthority uri) of
      ("ssh:", Just auth) -> uriUserInfo auth ++ uriRegName auth ++ ":" ++ uriPath uri ++ uriQuery uri ++ uriFragment uri
      (_, _) -> show uri

mustParseURI :: String -> URI
mustParseURI s = maybe (error ("Git - failed to parse URI: " ++ show s)) id (parseURI s)
