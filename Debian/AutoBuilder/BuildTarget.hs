{-# LANGUAGE CPP, FlexibleContexts, GADTs, OverloadedStrings, PackageImports, RankNTypes, ScopedTypeVariables #-}
module Debian.AutoBuilder.BuildTarget
    ( retrieve
    , targetDocumentation
    ) where

import Control.Exception (Exception)
import Control.Monad.Catch (MonadMask)
import Control.Monad.Except (lift, liftIO)
import Data.List (intersperse)
import qualified Debian.AutoBuilder.BuildTarget.Apt as Apt
import qualified Debian.AutoBuilder.BuildTarget.Cd as Cd
import qualified Debian.AutoBuilder.BuildTarget.Darcs as Darcs
import qualified Debian.AutoBuilder.BuildTarget.DebDir as DebDir
import qualified Debian.AutoBuilder.BuildTarget.Debianize as Debianize
import qualified Debian.AutoBuilder.BuildTarget.Git as Git
import qualified Debian.AutoBuilder.BuildTarget.Hackage as Hackage
import qualified Debian.AutoBuilder.BuildTarget.Hg as Hg
import qualified Debian.AutoBuilder.BuildTarget.Proc as Proc
import qualified Debian.AutoBuilder.BuildTarget.Patch as Patch
import qualified Debian.AutoBuilder.BuildTarget.Quilt as Quilt
import qualified Debian.AutoBuilder.BuildTarget.SourceDeb as SourceDeb
import qualified Debian.AutoBuilder.BuildTarget.Svn as Svn
import qualified Debian.AutoBuilder.BuildTarget.Tla as Tla
import qualified Debian.AutoBuilder.BuildTarget.Bzr as Bzr
import qualified Debian.AutoBuilder.BuildTarget.Uri as Uri
import qualified Debian.AutoBuilder.BuildTarget.Twice as Twice
import qualified Debian.AutoBuilder.Types.CacheRec as C
import Debian.AutoBuilder.Types.Download (Download(..), SomeDownload(..))
import qualified Debian.AutoBuilder.Types.Download as T
import qualified Debian.AutoBuilder.Types.Packages as P
import Debian.Debianize (CabalT, CabalInfo)
import Debian.Relation (SrcPkgName(..))
import qualified Debian.Repo.Fingerprint as P
import Debian.Repo.MonadOS (MonadOS)
import Debian.Repo.Rsync (HasRsyncError)
import Debian.Repo.SourceTree (HasSourceTree, SourceTree(dir'), copySourceTree, findSourceTree, topdir)
import Debian.Repo.Top (MonadTop)
import Extra.Except
import System.FilePath ((</>))
import System.Unix.Mount (WithProcAndSys)

data CdDL a
    = CdDL { _cd :: P.RetrieveMethod
           , _fs :: [P.PackageFlag]
           , _dir :: FilePath
           , _parent :: a
           } deriving Show

instance Download a => Download (CdDL a) where
    method = _cd
    flags = _fs
    getTop x = getTop (_parent x) </> _dir x
    logText x = logText (_parent x) ++ " (in subdirectory " ++ _dir x ++ ")"
    flushSource x = flushSource (_parent x)
    cleanTarget x = cleanTarget (_parent x)
    attrs = attrs . _parent

data ProcDL a
    = ProcDL { _procMethod :: P.RetrieveMethod
             , _procFlags :: [P.PackageFlag]
             , _base :: a
             } deriving Show

instance Download a => Download (ProcDL a) where
    method = _procMethod
    flags = _procFlags
    getTop = getTop . _base
    logText x = logText (_base x) ++ " (with /proc mounted)"
    flushSource x = flushSource (_base x)
    cleanTarget = cleanTarget . _base
    -- A no op - as of now we always mount /proc and /sys
    buildWrapper _ = id -- getOS >>= \ os -> withProcAndSys (rootPath . osRoot $ os) go
    attrs = attrs . _base
    origTarball x = origTarball (_base x)

data DirDL
    = DirDL { dirMethod :: P.RetrieveMethod
            , dirFlags :: [P.PackageFlag]
            , dirTree :: SourceTree } deriving Show

instance Download DirDL where
    method = dirMethod
    flags = dirFlags
    getTop = topdir . dirTree
    logText x = "Built from local directory " ++ show (method x)
    flushSource _ = return ()

data ZeroDL = ZeroDL deriving Show

instance Download ZeroDL where
    method _ = P.Zero
    flags _ = mempty
    getTop _ = error "getTop Zero"
    logText _ = error "logText Zero"
    flushSource _ = return ()

-- | Given a RetrieveMethod, perform the retrieval and return the result.
-- This wrapper ensures that /proc and /sys are mounted, even though the
-- underlying code in retrieve' hasn't been updated to enforce this.
retrieve :: forall r s e m. (MonadIOError e m, HasLoc e, MonadMask m, Exception e, HasRsyncError e, MonadOS r s m, MonadTop r m, HasSourceTree SourceTree m) =>
            CabalT IO () -> C.CacheRec -> P.RetrieveMethod -> [P.PackageFlag] -> [CabalInfo -> CabalInfo] -> WithProcAndSys m SomeDownload
retrieve defaultAtoms cache method' flags' functions = lift $ retrieve' defaultAtoms cache method' flags' functions

retrieve' :: forall r s e m. (MonadIOError e m, HasLoc e, MonadMask m, Exception e, HasRsyncError e, MonadOS r s m, MonadTop r m, HasSourceTree SourceTree m) =>
            CabalT IO () -> C.CacheRec -> P.RetrieveMethod -> [P.PackageFlag] -> [CabalInfo -> CabalInfo] -> m SomeDownload
retrieve' defaultAtoms cache method flags functions =
    case method of
      P.Apt dist package -> Apt.prepare cache method flags dist (SrcPkgName package)
      P.Bzr string -> Bzr.prepare cache method flags string

      P.Cd dir spec' ->
          retrieve' defaultAtoms cache spec' flags functions >>= \ target' ->
          return $ SomeDownload $ CdDL { _cd = method, _fs = flags, _dir = dir, _parent = target' }

      P.Darcs uri -> Darcs.prepare method flags uri

      P.DataFiles base files loc ->
          do base' <- retrieve' defaultAtoms cache base flags functions
             files' <- retrieve' defaultAtoms cache files flags functions
             baseTree <- liftIO (findSourceTree (T.getTop base') :: IO SourceTree)
             filesTree <- liftIO (findSourceTree (T.getTop files') :: IO SourceTree)
             _ <- liftIO $ copySourceTree filesTree (dir' baseTree </> loc)
             return base'

      P.DebDir upstream debian ->
          do upstream' <- retrieve' defaultAtoms cache upstream flags functions
             debian' <- retrieve' defaultAtoms cache debian flags functions
             DebDir.prepare method flags upstream' debian'
      P.Debianize _ -> error "retrieve - old Debianize constructor"
      P.Debianize'' package _ ->
          retrieve' defaultAtoms cache package flags functions >>=
          Debianize.prepare defaultAtoms cache method flags functions

      -- Dir is a simple instance of BuildTarget representing building the
      -- debian source in a local directory.  This type of target is used
      -- for testing, and is also returned by the clean method when the
      -- source control information has been stripped out of some other type
      -- of BuildTarget.
      P.Dir path ->
          do tree <- liftIO (findSourceTree path :: IO SourceTree)
             return $ SomeDownload $ DirDL { dirMethod = method
                                           , dirFlags = flags
                                           , dirTree = tree }

      P.Git uri specs -> Git.prepare method flags uri specs
      P.Hackage package -> Hackage.prepare cache method flags package
      P.Hg string -> Hg.prepare cache method flags string
      P.Patch base patch ->
          retrieve' defaultAtoms cache base flags functions >>=
          Patch.prepare method flags patch

      P.Proc spec' ->
          retrieve' defaultAtoms cache spec' flags functions >>= \ base ->
          return $ SomeDownload $ ProcDL { _procMethod = method
                                         , _procFlags = flags
                                         , _base = base }
      P.Quilt base patches ->
          do base' <- retrieve' defaultAtoms cache base flags functions
             patches' <- retrieve' defaultAtoms cache patches flags functions
             Quilt.prepare method flags base' patches'
      P.SourceDeb spec' ->
          retrieve' defaultAtoms cache spec' flags functions >>=
          SourceDeb.prepare cache method flags
      P.Svn uri -> Svn.prepare cache method flags uri
      P.Tla string -> Tla.prepare cache method flags string
      P.Twice base -> retrieve' defaultAtoms cache base flags functions >>=
                      Twice.prepare method flags
      P.Uri uri sum -> SomeDownload <$> Uri.prepare method flags uri sum
      P.Zero -> return $ SomeDownload ZeroDL

targetDocumentation :: String
targetDocumentation =
    "TARGET TYPES\n\nEach argument to --target describes a technique for obtaining\n" ++
    "the source code used to build a target.  The following target types are available:\n\n" ++
    concat (intersperse "\n\n" $
            map (concat . intersperse "\n  ")
            [ [ "dir:<path> - A target of this form simply uses whatever it finds on"
              , "the local machine at the given path as the debian source tree."
              , "Packages built using this targets are not allowed to be uploaded"
              , "since they include no revision control information." ]
            , Apt.documentation
            , Cd.documentation
            , Darcs.documentation
            , DebDir.documentation
            , Debianize.documentation
            , Hackage.documentation
            , Hg.documentation
            , Proc.documentation
            , Quilt.documentation
            , SourceDeb.documentation
            , Svn.documentation
            , Tla.documentation
            , Uri.documentation ])
