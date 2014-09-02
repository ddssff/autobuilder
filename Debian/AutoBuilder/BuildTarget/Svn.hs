{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Debian.AutoBuilder.BuildTarget.Svn 
    ( prepare
    , documentation
    ) where

import Control.Monad
import Control.Monad.Trans
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Digest.Pure.MD5 (md5)
import Data.List
import Data.Set (empty)
import qualified Debian.AutoBuilder.Types.Download as T
import qualified Debian.AutoBuilder.Types.CacheRec as P
import qualified Debian.AutoBuilder.Types.ParamRec as P
import qualified Debian.AutoBuilder.Types.Packages as P
import Debian.Repo
import Network.URI (URI(..), URIAuth(..), parseURI, unEscapeString)
import System.Directory
import System.Exit
import System.FilePath (splitFileName, (</>))
import System.Process (shell, proc)
import System.Process.Chunks (Chunk)
import System.Process.Read.Compat (timeTask)
import System.Process.Read.Convenience (keepStdout, keepStderr, keepResult)
import System.Unix.Directory

documentation :: [String]
documentation = [ "svn:<uri> - A target of this form retrieves the source code from"
                , "a subversion repository." ]

svn :: [String] -> IO [Chunk L.ByteString]
svn args = runProc (proc "svn" args)

username userInfo =
    let un = takeWhile (/= ':') userInfo in
    if null un
    then []
    else ["--username", unEscapeString un]

password userInfo =
    let pw = takeWhile (/= '@') . dropWhile (== ':') . dropWhile (/= ':') $ userInfo in
    if null pw
    then []
    else ["--password",unEscapeString pw]

prepare :: (MonadRepos m, MonadTop m) => P.CacheRec -> P.Packages -> String -> m T.Download
prepare cache package uri =
    do dir <- sub ("svn" </> show (md5 (L.pack (maybe "" uriRegName (uriAuthority uri') ++ (uriPath uri')))))
       when (P.flushSource (P.params cache)) (liftIO (removeRecursiveSafely dir))
       exists <- liftIO $ doesDirectoryExist dir
       tree <- liftIO $ if exists then verifySource dir else createSource dir
       return $ T.Download { T.package = package
                           , T.getTop = topdir tree
                           , T.logText =  "SVN revision: " ++ show (P.spec package)
                           , T.mVersion = Nothing
                           , T.origTarball = Nothing
                           , T.cleanTarget =
                               \ path ->
                                   case P.keepRCS package of
                                     False -> let cmd = "find " ++ path ++ " -name .svn -type d -print0 | xargs -0 -r -n1 rm -rf" in
                                              timeTask (runProc (shell cmd))
                                     True -> return ([], 0)
                           , T.buildWrapper = id
                           , T.attrs = empty
                           }
    where
      uri' = mustParseURI uri
      verifySource dir =
          svn (["status","--no-auth-cache","--non-interactive"] ++ (username userInfo) ++ (password userInfo)) >>= \ out ->
          case L.concat (keepStdout out ++ keepStderr out) == L.empty of
            -- no output == nothing changed
            True -> updateSource dir
            -- Failure - error code or output from status means changes have occured
            False ->  removeSource dir >> createSource dir

      removeSource dir = liftIO $ removeRecursiveSafely dir

      updateSource dir =
          do
            -- if the original url contained a specific revision, this will do the wrong thing
            _output <- svn (["update","--no-auth-cache","--non-interactive"] ++ (username userInfo) ++ (password userInfo))
            findSourceTree dir

      createSource dir =
          let (parent, _) = splitFileName dir in
          liftIO (createDirectoryIfMissing True parent) >>
          checkout dir >>
          findSourceTree dir :: IO SourceTree
      checkout :: FilePath -> IO (Either String [Chunk L.ByteString])
      --checkout = svn createStyle args 
      checkout dir = readProc (proc "svn" args) >>= return . finish
          where
            args = ([ "co","--no-auth-cache","--non-interactive"] ++ 
                    (username userInfo) ++ (password userInfo) ++ 
                    [ uri, dir ])
            finish output = case keepResult output of
                              [ExitSuccess] -> Right output
                              _ -> Left $ "*** FAILURE: svn " ++ concat (intersperse " " args)
      userInfo = maybe "" uriUserInfo (uriAuthority uri')

mustParseURI :: String -> URI
mustParseURI s = maybe (error ("Svn - parse failure: " ++ show s)) id (parseURI s)
