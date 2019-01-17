{-# LANGUAGE OverloadedStrings #-}
-- | Code pertaining to the local repository created to hold newly
-- built packages before uploading them to a remote repository.
module Debian.AutoBuilder.LocalRepo
    ( subDir
    , poolDir
    , prepare
    ) where

import Control.Lens (view)
import Control.Monad (when)
import Control.Monad.Trans (liftIO)
import Data.Set (Set)
import Debian.Arch (Arch)
import Debian.Release (ReleaseName, releaseName', parseSection')
import Debian.Repo.EnvPath (rootEnvPath)
import Debian.Repo.LocalRepository (LocalRepository)
import Debian.Repo.Release (Release(..), parseComponents)
import Debian.Repo.Internal.Repos (MonadRepos)
import Debian.Repo.State.Release (prepareRelease)
import Debian.Repo.State.Repository (prepareLocalRepository)
import Debian.Repo.Top (MonadTop, TopDir(TopDir), toTop)
import System.FilePath ((</>))
import System.Unix.Directory(removeRecursiveSafely)

subDir :: String
subDir = "localpools"

-- |Location of the local repository for uploaded packages.
poolDir :: MonadTop r m => ReleaseName -> m FilePath
poolDir rel = view toTop >>= \(TopDir top) -> return $ top </> subDir </> releaseName' rel

prepare :: (MonadRepos m, MonadTop r m) => Bool -> ReleaseName -> Set Arch -> m LocalRepository
prepare flush rel archset =
    do localRepo <- poolDir rel
       when flush (liftIO (removeRecursiveSafely localRepo))
       repo <- prepareLocalRepository (rootEnvPath localRepo) Nothing [defaultRelease]
       prepareRelease repo rel [] [parseSection' "main"] archset -- May be redundant now
       return repo
    where
      defaultRelease = Release { releaseName = rel
                               , releaseAliases = []
                               , releaseArchitectures = archset
                               , releaseComponents = parseComponents "main" }

