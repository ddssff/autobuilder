{-# LANGUAGE CPP, OverloadedStrings, PackageImports, ScopedTypeVariables, TypeFamilies #-}
-- |A 'uri:' target is an URI that returns a tarball, with an optional
-- md5sum if we want to ensure against the tarball changing unexpectedly.
module Debian.AutoBuilder.BuildTarget.Uri
    ( documentation
    , prepare
    , tarball
    , sourceDir
    ) where

import Control.Exception (SomeException)
import Control.Monad.Catch (catch)
import Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Lazy.Char8 as B (empty, readFile)
import Data.Digest.Pure.MD5 (md5)
import Data.List (isPrefixOf)
#if !MIN_VERSION_base(4,8,0)
import Data.Monoid (mempty)
#endif
import qualified Debian.AutoBuilder.Types.Download as T
import qualified Debian.AutoBuilder.Types.Packages as P
import qualified Debian.Repo as R (topdir, SourceTree, findSourceTree)
import Debian.Repo.Fingerprint (RetrieveMethod)
import Debian.Repo.Internal.Repos (MonadRepos)
import Debian.Repo.Prelude.Process (readProcessV, timeTask)
import Debian.Repo.Top (MonadTop, sub)
import Debian.URI
import Magic
import System.FilePath (splitFileName, (</>))
import System.Directory
import System.Process (shell)
import System.Unix.Directory

documentation :: [String]
documentation = [ "uri:<string>:<md5sum> - A target of this form retrieves the file at the"
                , "given URI, which is assumed to be a gzipped tarball.  The optional md5sum"
                , "suffix causes the build to fail if the downloaded file does not match"
                , "this checksum.  This prevents builds when the remote tarball has changed." ]

data UriDL
    = UriDL { method :: RetrieveMethod
            , flags :: [P.PackageFlag]
            , u :: String
            , s :: String
            , uri :: URI
            , cksum :: FilePath
            , tree :: R.SourceTree
            , tar :: FilePath
            } deriving Show

instance T.Download UriDL where
    method = method
    flags = flags
    getTop = R.topdir . tree
    flushSource _ = error "UriDL flushSource unimplemented"
    logText x = "Built from URI download " ++ uriToString' (uri x)
    origTarball x = Just (tar x)

-- | A URI that returns a tarball, with an optional md5sum which must
-- match if given.  The purpose of the md5sum is to be able to block
-- changes to the tarball on the remote host.
prepare :: (MonadRepos m, MonadTop r m) => RetrieveMethod -> [P.PackageFlag] -> String -> String -> m UriDL
prepare method flags u s =
    do (uri, cksum, tree) <- checkTarget >>= downloadTarget >> validateTarget >>= unpackTarget
       tar <- tarball (uriToString' uri) cksum
       return $ UriDL { method = method
                      , flags = flags
                      , u = u
                      , s = s
                      , uri = uri
                      , cksum = cksum
                      , tree = tree
                      , tar = tar }
    where
      checkTarget :: (MonadRepos m, MonadTop r m) => m Bool
      checkTarget =
          do tar <- tarball u s
             exists <- liftIO (doesFileExist tar)
             case exists of
               True ->
                   (liftIO (B.readFile tar >>= return . show . md5) >>= \ realSum ->
                    if realSum == s then return True else error "checksum mismatch")
                     `catch` (\ (_ :: SomeException) -> liftIO (removeRecursiveSafely tar) >> return False)
               False -> return False

      -- See if the file is already available in the checksum directory
      -- Download the target into the tmp directory, compute its checksum, and see if it matches.
      downloadTarget :: (MonadRepos m, MonadTop r m) => Bool -> m ()
      downloadTarget True = return ()
      downloadTarget False =
          do sum <- sumDir s
             tar <- tarball u s
             liftIO $ createDirectoryIfMissing True sum
             exists <- liftIO $ doesFileExist tar
             _output <-
                 case exists of
                   True -> return mempty
                   False -> readProcessV (shell ("curl -s '" ++ uriToString' (mustParseURI u) ++ "' > '" ++ tar ++ "'")) B.empty
             -- We should do something with the output
             return ()
      -- Make sure what we just downloaded has the correct checksum
      validateTarget :: (MonadRepos m, MonadTop r m) => m String
      validateTarget =
          (tarball u s) >>= \ tar ->
          (liftIO (B.readFile tar >>= return . show . md5) >>= \ realSum ->
           if realSum == s then return realSum else error ("Checksum mismatch for " ++ tar ++ ": expected " ++ s ++ ", saw " ++ realSum ++ "."))
            `catch` (\ (e :: SomeException) -> error ("Checksum failure for " ++ tar ++ ": " ++ show e))
      unpackTarget :: (MonadRepos m, MonadTop r m) => String -> m (URI, FilePath, R.SourceTree)
      unpackTarget realSum =
          rmdir >> mkdir >> untar >>= read >>= search >>= verify
          where
            rmdir = sourceDir s >>= \ src -> (liftIO (removeDirectoryRecursive src) `catch` (\ (_ :: SomeException) -> return ()))
            -- Create the unpack directory
            mkdir = sourceDir s >>= \ src ->
                    (liftIO (createDirectoryIfMissing True src) `catch` (\ (e :: SomeException) -> error ("Could not create " ++ src ++ ": " ++ show e)))
            untar =
                do magic <- liftIO $ magicOpen []
                   liftIO $ magicLoadDefault magic
                   tar <- tarball u s
                   src <- sourceDir s
                   fileInfo <- liftIO $ magicFile magic tar
                   case () of
                     _ | isPrefixOf "Zip archive data" fileInfo ->
                           liftIO $ timeTask $ readProcessV (shell ("unzip " ++ tar ++ " -d " ++ src)) B.empty
                       | isPrefixOf "gzip" fileInfo ->
                           liftIO $ timeTask $ readProcessV (shell ("tar xfz " ++ tar ++ " -C " ++ src)) B.empty
                       | isPrefixOf "bzip2" fileInfo ->
                           liftIO $ timeTask $ readProcessV (shell ("tar xfj " ++ tar ++ " -C " ++ src)) B.empty
                       | isPrefixOf "XZ compressed data" fileInfo ->
                           liftIO $ timeTask $ readProcessV (shell ("tar xfJ " ++ tar ++ " -C " ++ src)) B.empty
                       | True ->
                           liftIO $ timeTask $ readProcessV (shell ("cp " ++ tar ++ " " ++ src ++ "/")) B.empty
            read (_output, _elapsed) = sourceDir s >>= \ src -> liftIO (getDir src)
            getDir dir = getDirectoryContents dir >>= return . filter (not . flip elem [".", ".."])
            search files = checkContents (filter (not . flip elem [".", ".."]) files)
            checkContents :: (MonadRepos m, MonadTop r m) => [FilePath] -> m R.SourceTree
            checkContents [] = error ("Empty tarball? " ++ show (mustParseURI u))
            checkContents [subdir] =
                sourceDir s >>= \ src ->
                (liftIO (R.findSourceTree (src </> subdir))) `catch` (\ (_ :: SomeException) -> liftIO (R.findSourceTree src))
            checkContents _ = sourceDir s >>= \ src -> liftIO (R.findSourceTree src)
            verify tree = return (mustParseURI u, realSum, tree)

sumDir s = sub ("tmp" </> s)

tname u = snd . splitFileName . uriPath $ (mustParseURI u)

tarball u s = sumDir (s </> tname u)
sourceDir s = sumDir (s </> "unpack")

mustParseURI :: String -> URI
mustParseURI s = maybe (error ("Uri - parse failure: " ++ show s)) id (parseURI s)
