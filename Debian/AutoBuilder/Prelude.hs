{-# OPTIONS -Wall #-}
{-# LANGUAGE CPP #-}

module Debian.AutoBuilder.Prelude
    ( gFind
    , replaceFile
#if !MIN_VERSION_Cabal(2,0,0)
    , mkPackageName
    , mkVersion
#endif
    ) where

import Control.Exception (catch)
import Control.Monad.Reader (MonadPlus, msum)
import qualified Data.ByteString.Lazy as L (ByteString)
import Data.Generics (Data, Typeable, listify)
import Data.ListLike.IO (writeFile)
import Data.ListLike.Instances ({-instance ListLikeIO L.ByteString Word8-})
import Distribution.Package
#if MIN_VERSION_Cabal(2,0,0)
import Distribution.Version
#else
import Data.Version
#endif
import Prelude hiding (writeFile)
import System.Directory (removeFile)
import System.IO.Error (isDoesNotExistError)

-- | @gFind a@ will extract any elements of type @b@ from
-- @a@'s structure in accordance with the MonadPlus
-- instance, e.g. Maybe Foo will return the first Foo
-- found while [Foo] will return the list of Foos found.
gFind :: (MonadPlus m, Data a, Typeable b) => a -> m b
gFind = msum . map return . listify (const True)

-- Replace a file's contents, accounting for the possibility that the
-- old contents of the file may still be being read.  Apparently there
-- is a race condition in the file system so we may get one or more
-- isAlreadyBusyError exceptions before the writeFile succeeds.
replaceFile :: FilePath -> L.ByteString -> IO ()
replaceFile path text =
    --tries 100 10 f	-- There is now a fix for this problem, see ghc ticket 2122.
    f
    where
      f :: IO ()
      f = removeFile path `Control.Exception.catch` (\ e -> if isDoesNotExistError e then return () else ioError e) >> writeFile path text

#if !MIN_VERSION_Cabal(2,0,0)
mkPackageName = PackageName
mkVersion ns = Version ns []
#endif
