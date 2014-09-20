{-# LANGUAGE ExistentialQuantification #-}
-- |Modify a target so we cd to a subdirectory before building
module Debian.AutoBuilder.BuildTarget.Cd where

import Data.Set (empty)
import Debian.AutoBuilder.Types.Download (Download(..))
import qualified Debian.AutoBuilder.Types.CacheRec as P
import qualified Debian.AutoBuilder.Types.Packages as P
import Debian.Repo.Fingerprint (RetrieveMethod)
import Debian.Repo.Internal.Repos (MonadRepos)
import System.FilePath ((</>))

documentation = [ "cd:<relpath>:<target> - A target of this form modifies another target by"
                , "changing directories into a subdirectory before doing the build.  It is"
                , "used for repositories where the debian directory is in a subdirectory."]

prepare :: MonadRepos m => P.CacheRec -> RetrieveMethod -> [P.PackageFlag] -> FilePath -> Download -> m Download
prepare _cache method flags subdir target =
    do
    return $ Download { method = method
                      , flags = flags
                        , getTop = getTop target </> subdir
                        , logText = logText target ++ " (in subdirectory " ++ subdir ++ ")"
                        , mVersion = Nothing
                        , origTarball = Nothing
                        , cleanTarget = cleanTarget target
                        , buildWrapper = id
                        , attrs = empty
                        }
