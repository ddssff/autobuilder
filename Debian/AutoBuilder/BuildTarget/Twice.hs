-- |Modify a target so that dpkg-buildpackage is run again if it fails the first time.
module Debian.AutoBuilder.BuildTarget.Twice where

import qualified Debian.AutoBuilder.Types.Download as T
import qualified Debian.AutoBuilder.Types.Packages as P
import Debian.Repo.Internal.Repos (MonadRepos)

documentation = [ "twice:<target> - A target of this form modifies another target by"
                , "ensuring that dpkg-buildpackage is run a second time if it fails"
                , "the first time.  For some reason, certain packages are designed"
                , "to fail the first time to prevent fully automated builds."]

prepare :: MonadRepos m => P.Packages -> T.Download -> m T.Download
prepare package base =
    do return $ T.Download {
                    T.package = package
                  , T.getTop = T.getTop base
                  , T.logText = T.logText base ++ " (twice if necessary)"
                  , T.mVersion = Nothing
                  , T.origTarball = Nothing
                  , T.cleanTarget = T.cleanTarget base
                  -- This is a quick and dirty implementation, if you nest this inside another
                  -- target type it will have no effect.
                  , T.buildWrapper = \ action -> action >> action
                  }
