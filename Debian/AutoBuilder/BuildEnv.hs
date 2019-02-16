{-# LANGUAGE CPP, PackageImports, TemplateHaskell #-}
{-# OPTIONS -Wall #-}
module Debian.AutoBuilder.BuildEnv
    ( prepareDependOS
    , prepareBuildOS
    , envSet
    ) where

import Control.Lens (set)
import Control.Monad (when)
import Control.Monad.Catch (MonadMask)
import Control.Monad.Except (MonadError)
import Control.Monad.State (MonadIO)
import qualified Debian.AutoBuilder.LocalRepo as Local (prepare)
import qualified Debian.AutoBuilder.Types.ParamRec as P (ParamRec(archSet, buildRelease, cleanUp, components, excludePackages, flushDepends, flushPool, flushRoot, ifSourcesChanged, includePackages, optionalIncludePackages))
import Debian.Debianize (EnvSet(..))
import Debian.Codename (Codename, codename)
import Debian.Repo.EnvPath (EnvRoot(EnvRoot))
import Debian.Repo.MonadRepos (MonadRepos, putOSImage)
import Debian.Repo.OSImage (OSImage, osRoot)
import Debian.Repo.OSKey (OSKey(OSKey))
import Debian.Repo.Prelude.Verbosity (qPutStrLn)
import Debian.Repo.Rsync (HasRsyncError)
import Debian.Repo.Slice (NamedSliceList, Slice)
import Debian.Repo.State.OSImage (prepareOS)
import Debian.Repo.State.Package (deleteGarbage, evalInstall)
import Debian.Repo.Top (MonadTop, sub)
import Debian.TH (here)
import Debian.URI (HasParseError)
import Distribution.Pretty (prettyShow)
import Extra.Except
import Prelude hiding (null)
import System.FilePath ((</>))

envSet :: (MonadIO m, MonadTop r m) => Codename -> m EnvSet
envSet distro = sub ("dists" </> codename distro) >>= \ parent ->
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

prepareDependOS :: (MonadIOError e m, HasLoc e, Show e, HasRsyncError e, HasParseError e, MonadMask m,
                    MonadRepos s m, MonadTop r m) => P.ParamRec -> NamedSliceList -> [Slice] -> m OSKey
prepareDependOS params rel extra =
    do localRepo <- Local.prepare (P.flushPool params) (P.buildRelease params) (P.archSet params)
       -- release <- prepareRelease repo (P.buildRelease params) [] [parseSection' "main"] (P.archSet params)
       when (P.cleanUp params) (evalInstall deleteGarbage localRepo Nothing)
       eset <- envSet (P.buildRelease params)
       qPutStrLn (prettyShow $here <> " - prepareDependOS eset=" ++ show eset)
       -- exists <- liftIO $ doesDirectoryExist dRoot
       (_cOS, dOS) <- prepareOS eset rel extra localRepo (P.flushRoot params) (P.flushDepends params) (P.ifSourcesChanged params) (P.includePackages params) (P.optionalIncludePackages params) (P.excludePackages params) (P.components params)
       return dOS

prepareBuildOS :: (MonadIO m, MonadTop r m, MonadRepos s m) => Codename -> OSImage -> m OSKey
prepareBuildOS rel os = do
  r <- (OSKey . EnvRoot . buildOS) <$> envSet rel
  putOSImage (set osRoot r os)
  return r
