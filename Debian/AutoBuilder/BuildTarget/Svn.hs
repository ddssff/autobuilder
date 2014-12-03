{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TypeFamilies #-}
module Debian.AutoBuilder.BuildTarget.Svn 
    ( prepare
    , documentation
    ) where

import Control.Exception (SomeException)
import Control.Monad.Trans
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Digest.Pure.MD5 (md5)
import Data.List
import Data.Monoid ((<>), mempty)
import Data.String (IsString)
import qualified Debian.AutoBuilder.Types.Download as T
import qualified Debian.AutoBuilder.Types.CacheRec as P
import qualified Debian.AutoBuilder.Types.Packages as P
import Debian.Repo
import Debian.Repo.Fingerprint (RetrieveMethod)
import Debian.Repo.Prelude.Process (readProcessE, readProcessV, timeTask)
import Network.URI (URI(..), URIAuth(..), parseURI, unEscapeString)
import System.Directory
import System.Exit
import System.FilePath (splitFileName, (</>))
import System.Process (shell, proc)
import System.Unix.Directory

documentation :: [String]
documentation = [ "svn:<uri> - A target of this form retrieves the source code from"
                , "a subversion repository." ]

svn args = readProcessE (proc "svn" args) mempty

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

data SvnDL
    = SvnDL { cache :: P.CacheRec
            , method :: RetrieveMethod
            , flags :: [P.PackageFlag]
            , uri :: String
            , tree :: SourceTree }

instance T.Download SvnDL where
    method = method
    flags = flags
    getTop = topdir . tree
    logText x = "SVN revision: " ++ show (method x)
    flushSource _ = error "SvnDL flushSource unimplemented"
    cleanTarget x = (\ path ->
                         case any P.isKeepRCS (flags x) of
                           False -> let cmd = "find " ++ path ++ " -name .svn -type d -print0 | xargs -0 -r -n1 rm -rf" in
                                    timeTask (readProcessE (shell cmd) "")
                           True -> return (Right mempty, 0))

prepare :: (MonadRepos m, MonadTop m) => P.CacheRec -> RetrieveMethod -> [P.PackageFlag] -> String -> m T.SomeDownload
prepare cache method flags uri =
    do dir <- sub ("svn" </> show (md5 (L.pack (maybe "" uriRegName (uriAuthority uri') ++ (uriPath uri')))))
       exists <- liftIO $ doesDirectoryExist dir
       tree <- liftIO $ if exists then verifySource dir else createSource dir
       return $ T.SomeDownload $ SvnDL { cache = cache
                                       , method = method
                                       , flags = flags
                                       , uri = uri
                                       , tree = tree }
    where
      uri' = mustParseURI uri
      verifySource dir =
          svn (["status","--no-auth-cache","--non-interactive"] ++ (username userInfo) ++ (password userInfo)) >>= \ result ->
          case result of
            -- no output == nothing changed
            Right (ExitSuccess, out, err) | L.null (out <> err) -> updateSource dir
            -- Failure - error code or output from status means changes have occured
            _ -> removeSource dir >> createSource dir

      removeSource dir = liftIO $ removeRecursiveSafely dir

      updateSource dir =
          do
            -- if the original url contained a specific revision, this will do the wrong thing
            result <- svn (["update","--no-auth-cache","--non-interactive"] ++ (username userInfo) ++ (password userInfo))
            case (result :: Either SomeException (ExitCode, L.ByteString, L.ByteString)) of
              Right (ExitSuccess, _, _) -> findSourceTree dir
              _ -> error $ "svn -> " ++ show result

      createSource dir =
          let (parent, _) = splitFileName dir in
          liftIO (createDirectoryIfMissing True parent) >>
          checkout dir >>
          findSourceTree dir :: IO SourceTree
      checkout :: FilePath -> IO (Either String (ExitCode, L.ByteString, L.ByteString))
      --checkout = svn createStyle args
      checkout dir = readProcessE (proc "svn" args) "" >>= return . finish
          where
            args = ([ "co","--no-auth-cache","--non-interactive"] ++
                    (username userInfo) ++ (password userInfo) ++
                    [ uri, dir ])
            finish output =
                case output of
                  Right result@(ExitSuccess, _, _) -> Right result
                  _ -> Left $ "*** FAILURE: svn " ++ concat (intersperse " " args)
      userInfo = maybe "" uriUserInfo (uriAuthority uri')

mustParseURI :: String -> URI
mustParseURI s = maybe (error ("Svn - parse failure: " ++ show s)) id (parseURI s)
