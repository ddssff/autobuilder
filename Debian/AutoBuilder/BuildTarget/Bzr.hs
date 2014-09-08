-- | A Bazaar archive
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Debian.AutoBuilder.BuildTarget.Bzr where

import Control.Monad
import Control.Monad.Error (catchError, throwError)
import Control.Monad.Trans
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Digest.Pure.MD5 (md5)
import Data.List
import Data.Set (empty)
import qualified Debian.AutoBuilder.Types.CacheRec as P
import Debian.AutoBuilder.Types.Download (Download(..))
import qualified Debian.AutoBuilder.Types.ParamRec as P
import qualified Debian.AutoBuilder.Types.Packages as P
import Debian.Repo
import Debian.URI
import System.FilePath (splitFileName, (</>))
import System.Unix.Directory
import System.Process (shell)
import Debian.Repo.Prelude.Verbosity (timeTask, qPutStrLn)
import System.Directory
import System.Process.ListLike (collectProcessTriple)

documentation :: [String]
documentation = [ "bzr:<revision> - A target of this form retrieves the a Bazaar archive with the"
                , "given revision name." ]

prepare :: (MonadRepos m, MonadTop m) => P.CacheRec -> P.Packages -> String -> m Download
prepare cache package version =
  do
    dir <- askTop >>= \ top -> return $ top </> "bzr" </> show (md5 (L.pack (maybe "" uriRegName (uriAuthority uri) ++ (uriPath uri))))
    when (P.flushSource (P.params cache)) (liftIO (removeRecursiveSafely dir))
    exists <- liftIO $ doesDirectoryExist dir
    tree <- liftIO $ if exists then updateSource dir else createSource dir
    return $ Download
               { package = package
               , getTop = topdir tree
               , logText = "Bazaar revision: " ++ show (P.spec package)
               , mVersion = Nothing
               , origTarball = Nothing
               , cleanTarget = \ top ->
                   do qPutStrLn ("Clean Bazaar target in " ++ top)
                      case P.keepRCS package of
                        False -> let cmd = "find '" ++ top ++ "' -name '.bzr' -prune | xargs rm -rf" in
                                 timeTask (readProc (shell cmd) "")
                        True -> return ([], 0)
               , buildWrapper = id
               , attrs = empty }
    where
        -- Tries to update a pre-existant bazaar source tree
        updateSource dir =
            qPutStrLn ("Verifying Bazaar source archive '" ++ dir ++ "'") >>
            (readProc (shell cmd) "" >>= \ _ ->
             -- If we succeed then we try to merge with the parent tree
             mergeSource dir)
              -- if we fail then the source tree is corrupted, so get a new one
              `catchError` (\ e -> qPutStrLn (show e) >> removeSource dir >> createSource dir >> throwError e)
            where
                cmd   = "cd " ++ dir ++ " && ! `bzr status | grep -q 'modified:'`"

        -- computes a diff between this archive and some other parent archive and tries to merge the changes
        mergeSource dir =
            readProc (shell cmd) "" >>= return . collectProcessTriple >>= \ (_, out, _) ->
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
            readProc (shell cmd) "" >> findSourceTree dir
            where
                cmd   = "cd " ++ dir ++ " && bzr commit -m 'Merged Upstream'"
                -- style = (setStart (Just ("Commiting merge to local Bazaar source archive '" ++ dir ++ "'")) .
                --     setError (Just (\ _ -> "bzr commit failed in '" ++ dir ++ "'")))

        removeSource dir = liftIO $ removeRecursiveSafely dir

        createSource dir = do
            -- Create parent dir and let bzr create dir
            let (parent, _) = splitFileName dir
            createDirectoryIfMissing True parent
            _output <- readProc (shell cmd) ""
            findSourceTree dir :: IO SourceTree
            where
                cmd   = "bzr branch " ++ version ++ " " ++ dir
                -- style = (setStart (Just ("Retrieving Bazzar source for " ++ version)) .
                --     setError (Just (\ _ -> "bzr branch failed in " ++ dir)))
        uri = mustParseURI version
            where
                mustParseURI s = maybe (error ("Failed to parse URI: " ++ s)) id (parseURI s)

