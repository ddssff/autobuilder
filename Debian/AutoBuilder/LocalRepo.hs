{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
-- | Code pertaining to the local repository created to hold newly
-- built packages before uploading them to a remote repository.
module Debian.AutoBuilder.LocalRepo
    ( subDir
    , poolDir
    , prepare
    ) where

import Control.Lens (view)
import Control.Monad (when)
import Control.Monad.Except (liftIO, MonadError, MonadIO)
import Data.Set (Set)
import Debian.Arch (Arch)
import Debian.Codename (Codename, codename)
import Debian.Except (HasIOException)
import Debian.Release (parseSection')
import Debian.Repo.EnvPath (rootEnvPath)
import Debian.Repo.LocalRepository (LocalRepository)
import Debian.Repo.MonadRepos (MonadRepos)
import Debian.Repo.Prelude.Verbosity (qPutStrLn)
import Debian.Repo.Release (Release(..), parseComponents)
import Debian.Repo.State.Release (prepareRelease)
import Debian.Repo.State.Repository (prepareLocalRepository)
import Debian.Repo.Top (MonadTop, TopDir(TopDir), toTop)
import Debian.TH (here)
import Distribution.Pretty (prettyShow)
import System.FilePath ((</>))
import System.Unix.Directory(removeRecursiveSafely)

subDir :: String
subDir = "localpools"

-- |Location of the local repository for uploaded packages.
poolDir :: MonadTop r m => Codename -> m FilePath
poolDir rel = view toTop >>= \(TopDir top) -> return $ top </> subDir </> codename rel

prepare :: (MonadIO m, MonadRepos s m, MonadTop r m, HasIOException e, MonadError e m) => Bool -> Codename -> Set Arch -> m LocalRepository
prepare flush rel archset =
    do localRepo <- poolDir rel
       qPutStrLn (prettyShow $here <> " - localRepo=" ++ show localRepo)
       when flush (liftIO (removeRecursiveSafely localRepo))
       repo <- prepareLocalRepository [$here] (rootEnvPath localRepo) Nothing [defaultRelease]
       qPutStrLn (prettyShow $here <> " - repo=" ++ show repo)
       prepareRelease repo rel [] [parseSection' "main"] archset -- May be redundant now
       return repo
    where
      defaultRelease = Release { releaseName = rel
                               , releaseAliases = []
                               , releaseArchitectures = archset
                               , releaseComponents = parseComponents "main" }

