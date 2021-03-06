{-# LANGUAGE GADTs, TemplateHaskell #-}

-- |A SourceDeb target modifies another target to provide an unpacked debian source tree
-- when a debian source package is found.  A debian source package is a @.dsc@ file, a
-- @.tar.gz@ file, and an optional @.diff.gz@ file.

module Debian.AutoBuilder.BuildTarget.SourceDeb where

import Control.Monad.Trans
import qualified Data.ByteString.Lazy as B
import Data.List
import qualified Debian.AutoBuilder.Types.Download as T
import qualified Debian.AutoBuilder.Types.CacheRec as P
import qualified Debian.AutoBuilder.Types.Packages as P
import qualified Debian.Control.String as S
import Debian.Repo.Fingerprint (RetrieveMethod)
import Debian.TH (here)
import qualified Debian.Version as V
import Extra.Process (runV2)
import System.Directory
import System.Process (CreateProcess(cwd), proc)

documentation = [ "sourcedeb:<target> - A target of this form unpacks the source deb"
                , "retrieved by the original target and presents an unpacked source"
                , "tree for building.  Thus, the original target should retrieve a"
                , "directory containing a .dsc file, a .tar.gz, and an optional"
                , ".diff.gz file." ]

data SourceDebDL a
    = SourceDebDL { _method :: RetrieveMethod
                  , _flags :: [P.PackageFlag]
                  , _base :: a } deriving Show

instance T.Download a => T.Download (SourceDebDL a) where
    method = _method
    flags = _flags
    getTop = T.getTop . _base
    logText x = "Source Deb: " ++ show (_method x)
    flushSource _ = error "SourceDebDL flushSource unimplemented"
    attrs = T.attrs . _base

-- |Given the BuildTarget for the base target, prepare a SourceDeb BuildTarget
-- by unpacking the source deb.
prepare :: (MonadIO m, T.Download a) => P.CacheRec -> RetrieveMethod -> [P.PackageFlag] -> a -> m T.SomeDownload
prepare _cache method flags base =
    do dscFiles <- liftIO (getDirectoryContents top) >>= return . filter (isSuffixOf ".dsc")
       dscInfo <- mapM (\ name -> liftIO (readFile (top ++ "/" ++ name) >>= return . S.parseControl name)) dscFiles
       case sortBy compareVersions (zip dscFiles dscInfo) of
         [] -> return $  error ("Invalid sourcedeb base: no .dsc file in " ++ show (T.method base))
         (dscName, Right (S.Control (dscInfo' : _))) : _ ->
             let p = unpack top dscName in
             liftIO (runV2 [$here] p B.empty >>
                     makeTarget dscInfo' dscName)
         (dscName, _) : _ -> error ("Invalid .dsc file: " ++ dscName)
    where
      top = T.getTop base
      makeTarget dscInfo dscName =
          case (S.fieldValue "Source" dscInfo, maybe Nothing (Just . V.parseDebianVersion')
                     (S.fieldValue "Version" dscInfo)) of
            (Just _package, Just _version) ->
                return $ T.SomeDownload $ SourceDebDL { _method = method
                                                      , _flags = flags
                                                      , _base = T.SomeDownload base }
            _ -> error $ "Invalid .dsc file: " ++ dscName
      -- unpack top dscName = "cd " ++ top ++ " && dpkg-source -x " ++ dscName
      unpack top' dscName = (proc "dpkg-source" ["-x", dscName]) {cwd = Just top'}
      compareVersions (name2, info2) (name1, info1) =
          case (info1, info2) of
            (Right (S.Control (para1 : _)), Right (S.Control (para2 : _))) ->
                compare (maybe Nothing (Just . V.parseDebianVersion') (S.fieldValue "Version" para1))
                        (maybe Nothing (Just . V.parseDebianVersion') (S.fieldValue "Version" para2))
            _  -> error $ "Invalid .dsc file: " ++ name1 ++ " or " ++ name2
