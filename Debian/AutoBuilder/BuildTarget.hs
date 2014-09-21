{-# LANGUAGE GADTs, OverloadedStrings, PackageImports, RankNTypes, ScopedTypeVariables #-}
module Debian.AutoBuilder.BuildTarget
    ( retrieve
    , targetDocumentation
    ) where

import Control.Monad.Catch (MonadCatch)
import Control.Monad.Trans (liftIO)
import Data.List (intersperse)
import Data.Monoid (mempty)
import Data.Set (empty)
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
import Debian.AutoBuilder.Types.Download (Download(..), Download'(..), download')
import qualified Debian.AutoBuilder.Types.Download as T
import qualified Debian.AutoBuilder.Types.Packages as P
import Debian.Debianize (DebT)
import Debian.Relation (SrcPkgName(..))
import qualified Debian.Repo.Fingerprint as P
import Debian.Repo.MonadOS (MonadOS, withProc)
import Debian.Repo.SourceTree (SourceTree(dir'), copySourceTree, findSourceTree, topdir)
import Debian.Repo.Internal.Repos (MonadRepos)
import Debian.Repo.Top (MonadTop)
import System.FilePath ((</>))

-- | Given a RetrieveMethod, perform the retrieval and return the result.
retrieve :: forall m a. (MonadOS m, MonadRepos m, MonadTop m, MonadCatch m, Download a, a ~ Download') =>
            DebT IO () -> C.CacheRec -> P.RetrieveMethod -> [P.PackageFlag] -> m a
retrieve defaultAtoms cache method flags =
    case method of
      P.Apt dist package -> Apt.prepare cache method flags dist (SrcPkgName package)
      P.Bzr string -> Bzr.prepare cache method flags string

      P.Cd dir spec' ->
          retrieve defaultAtoms cache spec' flags >>= \ target' ->
          return $ download' {- T.method = -} method
                            {- , T.flags = -} flags
                            {- , T.getTop = -} (getTop target' </> dir)
                            {- , T.logText = -} (logText target' ++ " (in subdirectory " ++ dir ++ ")")
                            {- , T.mVersion = -} Nothing
                            {- , T.origTarball = -} Nothing
                            {- , T.cleanTarget = -} (cleanTarget target')
                            {- , T.buildWrapper = -} id
                            {- , T.attrs = -} (T.attrs target')

      P.Darcs uri -> Darcs.prepare cache method flags uri

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
      P.Debianize package ->
          retrieve defaultAtoms cache package flags >>=
          Debianize.prepare defaultAtoms cache method flags

      -- Dir is a simple instance of BuildTarget representing building the
      -- debian source in a local directory.  This type of target is used
      -- for testing, and is also returned by the clean method when the
      -- source control information has been stripped out of some other type
      -- of BuildTarget.
      P.Dir path ->
          do tree <- liftIO (findSourceTree path :: IO SourceTree)
             return $ download' {- { T.method = -} method
                                 {- , T.flags = -} flags
                                 {- , T.getTop = -} (topdir tree)
                                 {- , T.logText = -}  ("Built from local directory " ++ show method)
                                 {- , T.mVersion = -} Nothing
                                 {- , T.origTarball = -} Nothing
                                 {- , T.cleanTarget = -} (\ _ -> return ([], 0))
                                 {- , T.buildWrapper = -} id
                                 {- , T.attrs = -} empty

      P.Git uri specs -> Git.prepare cache method flags uri specs
      P.Hackage package -> Hackage.prepare cache method flags package
      P.Hg string -> Hg.prepare cache method flags string
      P.Patch base patch ->
          retrieve defaultAtoms cache base flags >>=
          Patch.prepare method flags patch

      P.Proc spec' ->
          retrieve defaultAtoms cache spec' flags >>= \ base ->
          return $ download'
                     {-   T.method = -} method
                     {- , T.flags = -} flags
                     {- , T.getTop = -} (T.getTop base)
                     {- , T.logText = -} (T.logText base ++ " (with /proc mounted)")
                     {- , T.mVersion = -} Nothing
                     {- , T.origTarball = -} Nothing
                     {- , T.cleanTarget = -} (T.cleanTarget base)
                     {- , T.buildWrapper = -} withProc
                     {- , T.attrs = -} (T.attrs base)
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
      P.Uri uri sum -> Uri.prepare cache method flags uri sum
      P.Zero -> return $ download' P.Zero mempty mempty mempty Nothing mempty undefined undefined mempty

{-
-- | Perform an IO operation with /proc mounted
withProc :: forall m a. (MonadOS m, MonadIO m) => m a -> m a
withProc task =
    rootPath <$> rootDir >>= \ root -> do liftIO $
      let dir = root </> "proc" in
      createDirectoryIfMissing True dir
      _ <- quieter 1 $ runProc (proc "mount" ["--bind", "/proc", dir])
      result <- try task :: IO (Either SomeException a)
      _ <- quieter 1 $ runProc (proc "umount" [dir])
      either throw return result
-}

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
