-- |Modify a target so that dpkg-buildpackage is run again if it fails the first time.
module Debian.AutoBuilder.BuildTarget.Twice where

import qualified Debian.AutoBuilder.Types.Download as T
import qualified Debian.AutoBuilder.Types.Packages as P
import Debian.Repo.Fingerprint (RetrieveMethod)
import Debian.Repo.Internal.Repos (MonadRepos)

documentation = [ "twice:<target> - A target of this form modifies another target by"
                , "ensuring that dpkg-buildpackage is run a second time if it fails"
                , "the first time.  For some reason, certain packages are designed"
                , "to fail the first time to prevent fully automated builds."]

data T.Download a => TwiceDL a
    = TwiceDL { method :: RetrieveMethod
              , flags :: [P.PackageFlag]
              , base :: a }

instance T.Download a => T.Download (TwiceDL a) where
    method = method
    flags = flags
    getTop = T.getTop . base
    logText x = T.logText (base x) ++ " (twice if necessary)"
    flushSource x = T.flushSource (base x)
    cleanTarget x = T.cleanTarget (base x)
    -- This is a quick and dirty implementation, if you nest this inside another
    -- target type it will have no effect.
    buildWrapper _ = (\ action -> action >> action)
    attrs = T.attrs . base

prepare :: (MonadRepos m, T.Download a) => RetrieveMethod -> [P.PackageFlag] -> a -> m T.SomeDownload
prepare method flags base =
    do return $ T.SomeDownload $ TwiceDL {method = method, flags = flags, base = T.SomeDownload base}
