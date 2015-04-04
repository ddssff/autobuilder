{-# LANGUAGE GADTs, OverloadedStrings, PackageImports, RankNTypes, ScopedTypeVariables #-}
module Debian.AutoBuilder.BuildTarget
    ( retrieve
    , targetDocumentation
    ) where

import Control.Applicative ((<$>))
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Trans (liftIO)
import Data.List (intersperse)
import Data.Monoid (mempty)
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
import Debian.Debianize (CabalT)
import Debian.Relation (SrcPkgName(..))
import Debian.Repo.EnvPath (EnvRoot(rootPath))
import qualified Debian.Repo.Fingerprint as P
import Debian.Repo.MonadOS (MonadOS, getOS)
import Debian.Repo.OSImage (osRoot)
import Debian.Repo.SourceTree (SourceTree(dir'), copySourceTree, findSourceTree, topdir)
import Debian.Repo.Internal.Repos (MonadRepos)
import Debian.Repo.Top (MonadTop)
import System.FilePath ((</>))
import System.FilePath.Extra4 (withProcAndSys)

data Download a => CdDL a
    = CdDL { cd :: P.RetrieveMethod
           , fs :: [P.PackageFlag]
           , dir :: FilePath
           , parent :: a
           }

instance Download a => Download (CdDL a) where
    method = cd
    flags = fs
    getTop x = getTop (parent x) </> dir x
    logText x = logText (parent x) ++ " (in subdirectory " ++ dir x ++ ")"
    flushSource x = flushSource (parent x)
    cleanTarget x = cleanTarget (parent x)
    attrs = attrs . parent

data Download a => ProcDL a
    = ProcDL { procMethod :: P.RetrieveMethod
             , procFlags :: [P.PackageFlag]
             , base :: a
             }

instance Download a => Download (ProcDL a) where
    method = procMethod
    flags = procFlags
    getTop = getTop . base
    logText x = logText (base x) ++ " (with /proc mounted)"
    flushSource x = flushSource (base x)
    cleanTarget = cleanTarget . base
    buildWrapper _ go = getOS >>= \ os -> withProcAndSys (rootPath . osRoot $ os) go
    attrs = attrs . base

data DirDL
    = DirDL { dirMethod :: P.RetrieveMethod
            , dirFlags :: [P.PackageFlag]
            , dirTree :: SourceTree }

instance Download DirDL where
    method = dirMethod
    flags = dirFlags
    getTop = topdir . dirTree
    logText x = "Built from local directory " ++ show (method x)
    flushSource _ = return ()

data ZeroDL = ZeroDL

instance Download ZeroDL where
    method _ = P.Zero
    flags _ = mempty
    getTop _ = error "getTop Zero"
    logText _ = error "logText Zero"
    flushSource _ = return ()

-- | Given a RetrieveMethod, perform the retrieval and return the result.
retrieve :: forall m. (MonadOS m, MonadRepos m, MonadTop m, MonadCatch m) =>
            CabalT IO () -> C.CacheRec -> P.RetrieveMethod -> [P.PackageFlag] -> m SomeDownload
retrieve defaultAtoms cache method flags =
    case method of
      P.Apt dist package -> Apt.prepare cache method flags dist (SrcPkgName package)
      P.Bzr string -> Bzr.prepare cache method flags string

      P.Cd dir spec' ->
          retrieve defaultAtoms cache spec' flags >>= \ target' ->
          return $ SomeDownload $ CdDL { cd = method, fs = flags, dir = dir, parent = target' }

      P.Darcs uri -> Darcs.prepare method flags uri

      P.DataFiles base files loc ->
          do base' <- retrieve defaultAtoms cache base flags
             files' <- retrieve defaultAtoms cache files flags
             baseTree <- liftIO (findSourceTree (T.getTop base') :: IO SourceTree)
             filesTree <- liftIO (findSourceTree (T.getTop files') :: IO SourceTree)
             _ <- liftIO $ copySourceTree filesTree (dir' baseTree </> loc)
             return base'

      P.DebDir upstream debian ->
          do upstream' <- retrieve defaultAtoms cache upstream flags
             debian' <- retrieve defaultAtoms cache debian flags
             DebDir.prepare method flags upstream' debian'
      P.Debianize _ -> error "retrieve - old Debianize constructor"
      P.Debianize'' package _ ->
          retrieve defaultAtoms cache package flags >>=
          Debianize.prepare defaultAtoms cache method flags

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
          retrieve defaultAtoms cache base flags >>=
          Patch.prepare method flags patch

      P.Proc spec' ->
          retrieve defaultAtoms cache spec' flags >>= \ base ->
          return $ SomeDownload $ ProcDL { procMethod = method
                                         , procFlags = flags
                                         , base = base }
      P.Quilt base patches ->
          do base' <- retrieve defaultAtoms cache base flags
             patches' <- retrieve defaultAtoms cache patches flags
             Quilt.prepare method flags base' patches'
      P.SourceDeb spec' ->
          retrieve defaultAtoms cache spec' flags >>=
          SourceDeb.prepare cache method flags
      P.Svn uri -> Svn.prepare cache method flags uri
      P.Tla string -> Tla.prepare cache method flags string
      P.Twice base -> retrieve defaultAtoms cache base flags >>=
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
