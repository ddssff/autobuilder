{-# LANGUAGE CPP, PackageImports #-}
{-# OPTIONS -Wall #-}
module Debian.AutoBuilder.BuildEnv
    ( prepareDependOS
    , prepareBuildOS
    , envSet
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative (Applicative)
#endif
import Control.Lens (set)
import Control.Monad (when)
import Control.Monad.Catch (MonadMask)
import Control.Monad.State (MonadIO)
import qualified Debian.AutoBuilder.LocalRepo as Local (prepare)
import qualified Debian.AutoBuilder.Types.ParamRec as P (ParamRec(archSet, buildRelease, cleanUp, components, excludePackages, flushDepends, flushPool, flushRoot, ifSourcesChanged, includePackages, optionalIncludePackages))
import Debian.Debianize (EnvSet(..))
import Debian.Release (ReleaseName, releaseName')
import Debian.Repo.EnvPath (EnvRoot(EnvRoot))
import Debian.Repo.Internal.Repos (MonadRepos, putOSImage)
import Debian.Repo.OSImage (OSImage, osRoot)
import Debian.Repo.Slice (NamedSliceList, Slice)
import Debian.Repo.State.OSImage (prepareOS)
import Debian.Repo.State.Package (deleteGarbage, evalInstall)
import Debian.Repo.Top (MonadTop, sub)
import Prelude hiding (null)
import System.FilePath ((</>))

envSet :: (MonadIO m, MonadTop r m) => ReleaseName -> m EnvSet
envSet distro = sub ("dists" </> releaseName' distro) >>= \ parent ->
                return (EnvSet {cleanOS = parent </> "clean", dependOS = parent </> "depend", buildOS = parent </> "build"})

{-
buildRoot :: (MonadIO m, MonadTop r m) => ReleaseName -> m EnvRoot
buildRoot distro = sub ("dists" </> releaseName' distro </> "build") >>= return . EnvRoot

dependRoot :: (MonadIO m, MonadTop r m) => ReleaseName -> m EnvRoot
dependRoot distro = sub ("dists" </> releaseName' distro </> "depend") >>= return . EnvRoot

cleanEnv :: (MonadIO m, MonadTop r m) => ReleaseName -> m EnvRoot
cleanEnv distro = cleanEnvOfRelease distro

cleanEnvOfRelease :: (MonadIO m, MonadTop r m) => ReleaseName -> m EnvRoot
cleanEnvOfRelease distro =
    sub ("dists" </> releaseName' distro </> "clean") >>= return . EnvRoot
-}

prepareDependOS :: (MonadRepos m, MonadTop r m) => P.ParamRec -> NamedSliceList -> [Slice] -> m EnvRoot
prepareDependOS params rel extra =
    do localRepo <- Local.prepare (P.flushPool params) (P.buildRelease params) (P.archSet params)
       -- release <- prepareRelease repo (P.buildRelease params) [] [parseSection' "main"] (P.archSet params)
       when (P.cleanUp params) (evalInstall deleteGarbage localRepo Nothing)
       eset <- envSet (P.buildRelease params)
       -- exists <- liftIO $ doesDirectoryExist dRoot
       (_cOS, dOS) <- prepareOS eset rel extra localRepo (P.flushRoot params) (P.flushDepends params) (P.ifSourcesChanged params) (P.includePackages params) (P.optionalIncludePackages params) (P.excludePackages params) (P.components params)
       return dOS

prepareBuildOS :: (MonadTop r m, MonadRepos m) => ReleaseName -> OSImage -> m EnvRoot
prepareBuildOS rel os = do
  r <- envSet rel >>= return . EnvRoot . buildOS
  putOSImage (set osRoot r os)
  return r
