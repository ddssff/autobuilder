{-# LANGUAGE ExistentialQuantification, GADTs #-}
-- |Modify a target so we cd to a subdirectory before building
module Debian.AutoBuilder.BuildTarget.Cd where

import Debian.AutoBuilder.Types.Download (Download(..), SomeDownload(..))
import qualified Debian.AutoBuilder.Types.CacheRec as P
import qualified Debian.AutoBuilder.Types.Packages as P
import Debian.Repo.Fingerprint (RetrieveMethod)
import Debian.Repo.Internal.Repos (MonadRepos)
import System.FilePath ((</>))

documentation = [ "cd:<relpath>:<target> - A target of this form modifies another target by"
                , "changing directories into a subdirectory before doing the build.  It is"
                , "used for repositories where the debian directory is in a subdirectory."]

data CdDL a
    = CdDL { cdMethod :: RetrieveMethod
           , cdFlags :: [P.PackageFlag]
           , subdir :: FilePath
           , cdParent :: a
           } deriving Show

instance Download a => Download (CdDL a) where
    method = cdMethod
    flags = cdFlags
    getTop x = getTop (cdParent x) </> subdir x
    logText x = logText (cdParent x) ++ " (in subdirectory " ++ subdir x ++ ")"
    flushSource = flushSource . cdParent
    cleanTarget = cleanTarget . cdParent

prepare :: (MonadRepos m, Download a) =>
           P.CacheRec -> RetrieveMethod -> [P.PackageFlag] -> FilePath -> a -> m SomeDownload
prepare _cache method flags subdir target =
    return $ SomeDownload $ CdDL { cdMethod = method
                                 , cdFlags = flags
                                 , subdir = subdir
                                 , cdParent = SomeDownload target }
