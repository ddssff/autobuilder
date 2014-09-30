{-# LANGUAGE ExistentialQuantification, PackageImports, RankNTypes, TypeFamilies #-}
{-# OPTIONS -fwarn-unused-imports #-}
module Debian.AutoBuilder.Types.Download
    ( Download(..)
    , SomeDownload(SomeDownload)
    ) where

import Control.Monad.Catch (MonadMask)
import Control.Monad.Trans (MonadIO)
import qualified Data.ByteString.Lazy as L (ByteString)
import Data.Set as Set (Set, empty)
import Data.Time (NominalDiffTime)
import Data.Version (Version)
import Debian.AutoBuilder.Types.Packages (PackageFlag)
import Debian.Repo.Fingerprint (RetrieveMethod(..), RetrieveAttribute(..))
import Debian.Repo.MonadOS (MonadOS)
import Debian.Repo.Top (MonadTop)
import System.Process.Chunks (Chunk)

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
    mVersion _ = Nothing
    -- ^ Some targets can return a cabal version, use this to retrieve it.
    origTarball :: a -> Maybe FilePath
    origTarball _ = Nothing
    -- ^ If we have access to an original tarball, this returns its path.
    flushSource :: (MonadIO m, MonadTop m) => a -> m ()
    -- ^ Remove any existing data before downloading anew
    cleanTarget :: a -> FilePath -> IO ([Chunk L.ByteString], NominalDiffTime)
    cleanTarget _ = \ _ -> return ([], 0)
    -- ^ Clean version control info out of a target after it has
    -- been moved to the given location.
    buildWrapper :: forall m. (MonadOS m, MonadMask m, MonadIO m) => a -> m NominalDiffTime -> m NominalDiffTime
    buildWrapper _ = id
    -- ^ Modify the build process in some way - currently only the
    -- proc target modifies this by mounting and then unmounting /proc.
    attrs :: a -> Set RetrieveAttribute
    attrs _ = empty
    -- ^ Attributes collected from performing the various retrieve
    -- methods

-- Existential type
data SomeDownload = forall a. Download a => SomeDownload a

instance Download SomeDownload where
    method (SomeDownload x) = method x
    flags (SomeDownload x) = flags x
    getTop (SomeDownload x) = getTop x
    logText (SomeDownload x) = logText x
    mVersion (SomeDownload x) = mVersion x
    origTarball (SomeDownload x) = origTarball x
    flushSource (SomeDownload x) = flushSource x
    cleanTarget (SomeDownload x) = cleanTarget x
    buildWrapper (SomeDownload x) = buildWrapper x
    attrs (SomeDownload x) = attrs x
