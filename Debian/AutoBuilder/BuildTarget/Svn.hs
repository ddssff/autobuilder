{-# LANGUAGE CPP, OverloadedStrings, ScopedTypeVariables, TemplateHaskell, TypeFamilies #-}
module Debian.AutoBuilder.BuildTarget.Svn 
    ( prepare
    , documentation
    ) where

import Control.Exception (Exception)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Trans
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Digest.Pure.MD5 (md5)
import Data.List
import Data.Monoid ((<>))
--import Data.String (IsString)
import qualified Debian.AutoBuilder.Types.Download as T
import qualified Debian.AutoBuilder.Types.CacheRec as P
import qualified Debian.AutoBuilder.Types.Packages as P
import Debian.Except (HasIOException(fromIOException), liftEIO)
import Debian.Repo
import Debian.Repo.Fingerprint (RetrieveMethod)
import Debian.Repo.Prelude.Process (runVE2, timeTask)
import Debian.TH (here)
import Network.URI (URI(..), URIAuth(..), parseURI, unEscapeString)
import System.Directory
import System.Exit
import System.FilePath (splitFileName, (</>))
import System.Process (shell, proc)
import System.Unix.Directory

documentation :: [String]
documentation = [ "svn:<uri> - A target of this form retrieves the source code from"
                , "a subversion repository." ]

svn ::
    (MonadCatch m, MonadIO m, Exception e, MonadError e m)
    => [String] -> m (Either e (ExitCode, L.ByteString, L.ByteString))
svn args = runVE2 $here (proc "svn" args) mempty

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
    = SvnDL { _cache :: P.CacheRec
            , _method :: RetrieveMethod
            , _flags :: [P.PackageFlag]
            , _uri :: String
            , _tree :: SourceTree } deriving Show

instance T.Download SvnDL where
    method = _method
    flags = _flags
    getTop = topdir . _tree
    logText x = "SVN revision: " ++ show (_method x)
    flushSource _ = error "SvnDL flushSource unimplemented"
    cleanTarget x = (\ path ->
                         case any P.isKeepRCS (_flags x) of
                           False -> let cmd = "find " ++ path ++ " -name .svn -type d -print0 | xargs -0 -r -n1 rm -rf" in
                                    timeTask (runVE2 $here (shell cmd) "")
                           True -> return (Right mempty, 0))

prepare :: forall r e m. (MonadIO m, MonadCatch m, MonadTop r m, Exception e, HasIOException e, MonadError e m) => P.CacheRec -> RetrieveMethod -> [P.PackageFlag] -> String -> m T.SomeDownload
prepare cache method flags uri =
    do dir <- sub ("svn" </> show (md5 (L.pack (maybe "" uriRegName (uriAuthority uri') ++ (uriPath uri')))))
       exists <- liftEIO $here $ doesDirectoryExist dir
       tree <- if exists then verifySource dir else createSource dir
       return $ T.SomeDownload $ SvnDL { _cache = cache
                                       , _method = method
                                       , _flags = flags
                                       , _uri = uri
                                       , _tree = tree }
    where
      uri' = mustParseURI uri
      verifySource :: forall m'. (MonadIO m', MonadCatch m', MonadError e m') => FilePath -> m' SourceTree
      verifySource dir = do
        (result :: Either e (ExitCode, L.ByteString, L.ByteString)) <- svn (["status","--no-auth-cache","--non-interactive"] ++ (username userInfo) ++ (password userInfo))
        case result of
          -- no output == nothing changed
          Right (ExitSuccess, out, err) | L.null (out <> err) -> updateSource dir
          -- Failure - error code or output from status means changes have occured
          _ -> removeSource dir >> createSource dir

      removeSource dir = liftEIO $here $ removeRecursiveSafely dir

      updateSource :: forall m'. (MonadIO m', MonadCatch m', MonadError e m') => FilePath -> m' SourceTree
      updateSource dir =
          do
            -- if the original url contained a specific revision, this will do the wrong thing
            result <- svn (["update","--no-auth-cache","--non-interactive"] ++ (username userInfo) ++ (password userInfo))
            case (result :: Either e (ExitCode, L.ByteString, L.ByteString)) of
              Right (ExitSuccess, _, _) -> liftEIO $here $ findSourceTree dir
              _ -> error $ "svn -> " ++ show result

      createSource :: forall m'. (MonadIO m', MonadError e m') => FilePath -> m' SourceTree
      createSource dir = liftEIO $here $
          let (parent, _) = splitFileName dir in
          liftIO (createDirectoryIfMissing True parent) >>
          checkout dir >>
          liftIO (findSourceTree dir)
      checkout :: forall e' m'. (MonadIO m', MonadCatch m', Exception e', HasIOException e', MonadError e' m') => FilePath -> m' (Either e' (ExitCode, L.ByteString, L.ByteString))
      --checkout = svn createStyle args
      checkout dir = runVE2 $here (proc "svn" args) "" >>= return . finish
          where
            args = ([ "co","--no-auth-cache","--non-interactive"] ++
                    (username userInfo) ++ (password userInfo) ++
                    [ uri, dir ])
            finish :: Either e' (ExitCode, L.ByteString, L.ByteString) -> Either e' (ExitCode, L.ByteString, L.ByteString)
            finish output =
                case output of
                  Right result@(ExitSuccess, _, _) -> Right result
                  _ -> throwError $ fromIOException $here $ userError $ "*** FAILURE: svn " ++ concat (intersperse " " args)
      userInfo = maybe "" uriUserInfo (uriAuthority uri')

mustParseURI :: String -> URI
mustParseURI s = maybe (error ("Svn - parse failure: " ++ show s)) id (parseURI s)
