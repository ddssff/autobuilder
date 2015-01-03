-- | A Bazaar archive
{-# LANGUAGE GADTs, OverloadedStrings, ScopedTypeVariables #-}
module Debian.AutoBuilder.BuildTarget.Bzr where

import Control.Monad.Error (catchError, throwError)
import Control.Monad.Trans
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Digest.Pure.MD5 (md5)
import Data.List
import Data.Monoid (mempty)
import qualified Debian.AutoBuilder.Types.CacheRec as P
import Debian.AutoBuilder.Types.Download (Download(..), SomeDownload(..))
import qualified Debian.AutoBuilder.Types.Packages as P
import Debian.Repo
import Debian.Repo.Fingerprint (RetrieveMethod)
import Debian.URI
import System.FilePath (splitFileName, (</>))
import System.Unix.Directory
import System.Process (shell)
import Debian.Repo.Prelude.Process (readProcessVE, readProcessV, timeTask)
import Debian.Repo.Prelude.Verbosity (qPutStrLn)
import System.Directory

documentation :: [String]
documentation = [ "bzr:<revision> - A target of this form retrieves the a Bazaar archive with the"
                , "given revision name." ]

data BzrDL
    = BzrDL
      { bzrCache :: P.CacheRec
      , bzrMethod :: RetrieveMethod
      , bzrFlags :: [P.PackageFlag]
      , bzrVersion :: String
      , bzrTree :: SourceTree
      }

instance Download BzrDL where
    method = bzrMethod
    flags = bzrFlags
    getTop = topdir . bzrTree
    logText x = "Bazaar revision: " ++ show (method x)
    flushSource _ = error "BzrDL flushSource unimplemented"
    cleanTarget x =
        \ top ->
            do qPutStrLn ("Clean Bazaar target in " ++ top)
               case any P.isKeepRCS (flags x) of
                 False -> let cmd = "find '" ++ top ++ "' -name '.bzr' -prune | xargs rm -rf" in
                          timeTask (readProcessVE (shell cmd) L.empty)
                 True -> return (Right mempty, 0)

prepare :: (MonadRepos m, MonadTop m) => P.CacheRec -> RetrieveMethod -> [P.PackageFlag] -> String -> m SomeDownload
prepare cache method flags version =
  do
    dir <- askTop >>= \ top -> return $ top </> "bzr" </> show (md5 (L.pack (maybe "" uriRegName (uriAuthority uri) ++ (uriPath uri))))
    exists <- liftIO $ doesDirectoryExist dir
    tree <- liftIO $ if exists then updateSource dir else createSource dir
    return $ SomeDownload $ BzrDL { bzrCache = cache
                                  , bzrMethod = method
                                  , bzrFlags = flags
                                  , bzrVersion = version
                                  , bzrTree = tree }
    where
        -- Tries to update a pre-existant bazaar source tree
        updateSource dir =
            qPutStrLn ("Verifying Bazaar source archive '" ++ dir ++ "'") >>
            (readProcessV (shell cmd) L.empty >>= \ _ ->
             -- If we succeed then we try to merge with the parent tree
             mergeSource dir)
              -- if we fail then the source tree is corrupted, so get a new one
              `catchError` (\ e -> qPutStrLn (show e) >> removeSource dir >> createSource dir >> throwError e)
            where
                cmd   = "cd " ++ dir ++ " && ! `bzr status | grep -q 'modified:'`"

        -- computes a diff between this archive and some other parent archive and tries to merge the changes
        mergeSource dir =
            readProcessV (shell cmd) L.empty >>= \ (_, out, _) ->
            if isInfixOf "Nothing to do." (L.unpack out)
            then findSourceTree dir :: IO SourceTree
            else commitSource dir
            where
                cmd   = "cd " ++ dir ++ " && bzr merge"
                -- style = (setStart (Just ("Merging local Bazaar source archive '" ++ dir ++ "' with parent archive")).
                --          setError (Just (\ _ -> "bzr merge failed in '" ++ dir ++ "'")))

        -- Bazaar is a distributed revision control system so you must commit to the local source
        -- tree after you merge from some other source tree
        commitSource dir =
            readProcessV (shell cmd) L.empty >> findSourceTree dir
            where
                cmd   = "cd " ++ dir ++ " && bzr commit -m 'Merged Upstream'"
                -- style = (setStart (Just ("Commiting merge to local Bazaar source archive '" ++ dir ++ "'")) .
                --     setError (Just (\ _ -> "bzr commit failed in '" ++ dir ++ "'")))

        removeSource dir = liftIO $ removeRecursiveSafely dir

        createSource dir = do
            -- Create parent dir and let bzr create dir
            let (parent, _) = splitFileName dir
            createDirectoryIfMissing True parent
            _output <- readProcessV (shell cmd) L.empty
            findSourceTree dir :: IO SourceTree
            where
                cmd   = "bzr branch " ++ version ++ " " ++ dir
                -- style = (setStart (Just ("Retrieving Bazzar source for " ++ version)) .
                --     setError (Just (\ _ -> "bzr branch failed in " ++ dir)))
        uri = mustParseURI version
            where
                mustParseURI s = maybe (error ("Failed to parse URI: " ++ s)) id (parseURI s)

