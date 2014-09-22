{-# LANGUAGE GADTs, ScopedTypeVariables #-}
module Debian.AutoBuilder.BuildTarget.Apt where

import Control.Monad.Trans (MonadIO(liftIO))
import Data.Maybe (mapMaybe)
import Data.Set (singleton)
import qualified Debian.AutoBuilder.Types.CacheRec as P (CacheRec(allSources, params))
import Debian.AutoBuilder.Types.Download (Download(..), SomeDownload(..))
import qualified Debian.AutoBuilder.Types.Packages as P (PackageFlag, aptPin)
import qualified Debian.AutoBuilder.Types.ParamRec as P (ParamRec(ifSourcesChanged))
import Debian.Changes (logVersion)
import Debian.Relation (SrcPkgName)
import Debian.Release (ReleaseName(ReleaseName, relName))
import Debian.Repo.AptImage (aptDir)
import Debian.Repo.Fingerprint (RetrieveMethod, RetrieveAttribute(AptVersion))
import Debian.Repo.Slice (NamedSliceList(sliceListName))
import Debian.Repo.SourceTree (topdir, DebianBuildTree, entry, debTree')
import Debian.Repo.Internal.Repos (MonadRepos)
import Debian.Repo.State.AptImage (withAptImage, prepareSource)
import Debian.Repo.Top (MonadTop)
import Debian.Version (parseDebianVersion, prettyDebianVersion)
import System.Unix.Directory (removeRecursiveSafely)

documentation = [ "apt:<distribution>:<packagename> - a target of this form looks up"
                , "the sources.list named <distribution> and retrieves the package with"
                , "the given name from that distribution." ]

data AptDL
    = AptDL { allSources :: [NamedSliceList]
            , aptMethod :: RetrieveMethod
            , aptFlags :: [P.PackageFlag]
            , dist :: String
            , package :: SrcPkgName
            , apt :: FilePath
            , tree :: DebianBuildTree }

instance Download AptDL where
    method = aptMethod
    flags = aptFlags
    getTop = topdir . tree
    logText x =
        "Built from " ++ relName (sliceListName distro) ++ " apt pool, apt-revision: " ++ show (method x)
        where
          distro = maybe (error $ "Invalid dist: " ++ relName dist') id (findRelease (allSources x) dist')
          dist' = ReleaseName (dist x)
          findRelease distros dist =
              case filter ((== dist) . sliceListName) distros of
                [a] -> Just a
                [] -> Nothing
                a -> error $ ("Multiple sources.lists found for " ++ relName dist ++ ": " ++ show (map (relName . sliceListName) a))
    flushSource x = liftIO $ removeRecursiveSafely $ apt x
    attrs = singleton . AptVersion . show . prettyDebianVersion . logVersion . entry . debTree' . tree

prepare :: (MonadRepos m, MonadTop m) => P.CacheRec -> RetrieveMethod -> [P.PackageFlag] -> String -> SrcPkgName -> m SomeDownload
prepare cache method flags dist package =
    withAptImage (P.ifSourcesChanged (P.params cache)) distro $ do
      apt <- aptDir package
      tree <- prepareSource package version'
      return $ SomeDownload $ AptDL { allSources = P.allSources cache
                                    , aptMethod = method
                                    , aptFlags = flags
                                    , dist = dist
                                    , package = package
                                    , apt = apt
                                    , tree = tree }
    where
      distro = maybe (error $ "Invalid dist: " ++ relName dist') id (findRelease (P.allSources cache) dist')
      dist' = ReleaseName dist
      version' = case mapMaybe P.aptPin flags of
                   [] -> Nothing
                   [v] -> Just (parseDebianVersion v)
                   vs -> error ("Conflicting pin versions for apt-get: " ++ show vs)
      findRelease distros dist =
          case filter ((== dist) . sliceListName) distros of
            [a] -> Just a
            [] -> Nothing
            a -> error $ ("Multiple sources.lists found for " ++ relName dist ++ ": " ++ show (map (relName . sliceListName) a))
