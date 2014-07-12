{-# LANGUAGE ScopedTypeVariables #-}
-- |Modify a target so that \/proc is mounted while it builds.
module Debian.AutoBuilder.BuildTarget.Proc where

--import Control.Monad.Catch (MonadCatch)
--import Control.Monad.State (evalStateT)
--fimport Control.Monad.Trans (MonadIO)
--import qualified Debian.AutoBuilder.Types.Download as T
--import qualified Debian.AutoBuilder.Types.CacheRec as P
--import qualified Debian.AutoBuilder.Types.Packages as P
--import Debian.Repo

documentation = [ "proc:<target> - A target of this form modifies another target by ensuring"
                , "that /proc is mounted during the build.  This target should only be"
                , "used if absolutely necessary, because it reveals details of the build"
                , "machine which might be different from the machine on which the package"
                , "is ultimately installed." ]

{-
prepare :: forall m. (MonadRepos m, MonadCatch m) => P.CacheRec -> P.Packages -> OSImage -> T.Download -> m T.Download
prepare _cache package buildOS base =
    return $ T.Download {
                 T.package = package
               , T.getTop = T.getTop base
               , T.logText = T.logText base ++ " (with /proc mounted)"
               , T.mVersion = Nothing
               , T.origTarball = Nothing
               , T.cleanTarget = T.cleanTarget base
               , T.buildWrapper = withProc -- \ (task :: MonadRepos m, MonadOS m, MonadCatch m => m a) -> evalStateT (withProc task) buildOS
               }
-}