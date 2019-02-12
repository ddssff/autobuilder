{-# LANGUAGE GADTs, ScopedTypeVariables, TemplateHaskell #-}
module Debian.AutoBuilder.BuildTarget.Apt where

import Control.Monad.Catch (MonadCatch)
import Control.Monad.Except (MonadIO(liftIO), MonadError)
import Data.Maybe (mapMaybe)
import Data.Set (singleton)
import qualified Debian.AutoBuilder.Types.CacheRec as P (CacheRec(allSources, params))
import Debian.AutoBuilder.Types.Download (Download(..), SomeDownload(..))
import qualified Debian.AutoBuilder.Types.Packages as P (PackageFlag, aptPin)
import qualified Debian.AutoBuilder.Types.ParamRec as P (ParamRec(ifSourcesChanged))
import Debian.Changes (logVersion)
import Debian.Codename (codename, parseCodename)
import Debian.Except (HasIOException)
import Debian.Relation (SrcPkgName)
import Debian.Repo.AptImage (aptDir)
import Debian.Repo.Fingerprint (RetrieveMethod, RetrieveAttribute(AptVersion))
import Debian.Repo.Slice (NamedSliceList(sliceListName))
import Debian.Repo.SourceTree (topdir, DebianBuildTree, entry, debTree')
import Debian.Repo.MonadRepos (MonadRepos)
import Debian.Repo.State.AptImage (withAptImage, prepareSource)
import Debian.Repo.Top (MonadTop)
import Debian.TH (here)
import Debian.Version (parseDebianVersion', prettyDebianVersion)
import System.Unix.Directory (removeRecursiveSafely)

documentation = [ "apt:<distribution>:<packagename> - a target of this form looks up"
                , "the sources.list named <distribution> and retrieves the package with"
                , "the given name from that distribution." ]

data AptDL
    = AptDL { _allSources :: [NamedSliceList]
            , _aptMethod :: RetrieveMethod
            , _aptFlags :: [P.PackageFlag]
            , _dist :: String
            , _package :: SrcPkgName
            , _apt :: FilePath
            , _tree :: DebianBuildTree } deriving Show

instance Download AptDL where
    method = _aptMethod
    flags = _aptFlags
    getTop = topdir . _tree
    logText x =
        "Built from " ++ codename (sliceListName distro) ++ " apt pool, apt-revision: " ++ show (method x)
        where
          distro = maybe (error $ "Invalid dist: " ++ codename dist') id (findRelease (_allSources x) dist')
          dist' = parseCodename (_dist x)
          findRelease distros name =
              case filter ((== name) . sliceListName) distros of
                [a] -> Just a
                [] -> Nothing
                a -> error $ ("Multiple sources.lists found for " ++ codename name ++ ": " ++ show (map (codename . sliceListName) a))
    flushSource x = liftIO $ removeRecursiveSafely $ _apt x
    attrs = singleton . AptVersion . show . prettyDebianVersion . logVersion . entry . debTree' . _tree

prepare ::
    (MonadIO m, MonadCatch m, MonadRepos s m, MonadTop r m, HasIOException e, MonadError e m)
    => P.CacheRec -> RetrieveMethod -> [P.PackageFlag] -> String -> SrcPkgName -> m SomeDownload
prepare cache method' flags' name package =
    withAptImage [$here] (P.ifSourcesChanged (P.params cache)) distro $ do
      apt <- aptDir package
      tree <- prepareSource package version'
      return $ SomeDownload $ AptDL { _allSources = P.allSources cache
                                    , _aptMethod = method'
                                    , _aptFlags = flags'
                                    , _dist = name
                                    , _package = package
                                    , _apt = apt
                                    , _tree = tree }
    where
      distro = maybe (error $ "Invalid dist: " ++ codename dist') id (findRelease (P.allSources cache) dist')
      dist' = parseCodename name
      version' = case mapMaybe P.aptPin flags' of
                   [] -> Nothing
                   [v] -> Just (parseDebianVersion' v)
                   vs -> error ("Conflicting pin versions for apt-get: " ++ show vs)
      findRelease distros name' =
          case filter ((== name') . sliceListName) distros of
            [a] -> Just a
            [] -> Nothing
            a -> error $ ("Multiple sources.lists found for " ++ codename name' ++ ": " ++ show (map (codename . sliceListName) a))
