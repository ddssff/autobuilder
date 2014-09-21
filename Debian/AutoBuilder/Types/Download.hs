{-# LANGUAGE PackageImports, RankNTypes, TypeFamilies #-}
{-# OPTIONS -fwarn-unused-imports #-}
module Debian.AutoBuilder.Types.Download
    ( Download' -- (..)
    , download'
    , Download(..)
    ) where

import Control.Monad.Catch (MonadMask)
import Control.Monad.Trans (MonadIO)
import qualified Data.ByteString.Lazy as L (ByteString)
import Data.Set as Set (Set)
import Data.Time (NominalDiffTime)
import Data.Version (Version)
import Debian.AutoBuilder.Types.Packages (PackageFlag)
import Debian.Repo.Fingerprint (RetrieveMethod(..), RetrieveAttribute(..))
import Debian.Repo.MonadOS (MonadOS)
import System.Process.ListLike (Chunk)

data Download'
    = Download
      { method' :: RetrieveMethod
      -- ^ The method used to retrieve this target.
      , flags' :: [PackageFlag]
      -- ^ The flags assocated with the package
      , getTop' :: FilePath
      -- ^ The directory containing the target's files.  For most target types, these
      --  files could be anything, not necessarily a Debian source directory.
      , logText' :: String
      -- ^ Text to include in changelog entry.
      , mVersion' :: Maybe Version
      -- ^ Some targets can return a cabal version, use this to retrieve it.
      , origTarball' :: Maybe FilePath
      -- ^ If we have access to an original tarball, this returns its path.
      , cleanTarget' :: FilePath -> IO ([Chunk L.ByteString], NominalDiffTime)
      -- ^ Clean version control info out of a target after it has
      -- been moved to the given location.
      , buildWrapper' :: forall m. (MonadOS m, MonadMask m, MonadIO m) => m NominalDiffTime -> m NominalDiffTime
      -- ^ Modify the build process in some way - currently only the
      -- proc target modifies this by mounting and then unmounting /proc.
      , attrs' :: Set RetrieveAttribute
      -- ^ Attributes collected from performing the various retrieve
      -- methods
      }

class Download a where
    method :: a -> RetrieveMethod
    -- ^ The method used to retrieve this target.
    flags :: a -> [PackageFlag]
    -- ^ The flags assocated with the package
    getTop :: a -> FilePath
    -- ^ The directory containing the target's files.  For most target types, these
    --  files could be anything, not necessarily a Debian source directory.
    logText :: a -> String
    -- ^ Text to include in changelog entry.
    mVersion :: a -> Maybe Version
    -- ^ Some targets can return a cabal version, use this to retrieve it.
    origTarball :: a -> Maybe FilePath
    -- ^ If we have access to an original tarball, this returns its path.
    cleanTarget :: a -> FilePath -> IO ([Chunk L.ByteString], NominalDiffTime)
    -- ^ Clean version control info out of a target after it has
    -- been moved to the given location.
    buildWrapper :: forall m. (MonadOS m, MonadMask m, MonadIO m) => a -> m NominalDiffTime -> m NominalDiffTime
    -- ^ Modify the build process in some way - currently only the
    -- proc target modifies this by mounting and then unmounting /proc.
    attrs :: a -> Set RetrieveAttribute
    -- ^ Attributes collected from performing the various retrieve
    -- methods

instance Download Download' where
    method = method'
    flags = flags'
    getTop = getTop'
    logText = logText'
    mVersion = mVersion'
    origTarball = origTarball'
    cleanTarget = cleanTarget'
    buildWrapper = buildWrapper'
    attrs = attrs'

-- Temporary constructor
download' = Download
