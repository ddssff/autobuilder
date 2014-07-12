{-# LANGUAGE OverloadedStrings, PackageImports, RankNTypes, ScopedTypeVariables #-}
module Debian.AutoBuilder.BuildTarget
    ( retrieve
    , targetDocumentation
    ) where

import Control.Monad.Catch (MonadCatch)
import Control.Monad.Trans (liftIO)
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
import Debian.AutoBuilder.Types.Download (Download(..))
import qualified Debian.AutoBuilder.Types.Download as T
import qualified Debian.AutoBuilder.Types.Packages as P
import Debian.Debianize (DebT)
import Debian.Relation (SrcPkgName(..))
import Debian.Repo.MonadOS (MonadOS, withProc)
import Debian.Repo.SourceTree (SourceTree(dir'), copySourceTree, findSourceTree, topdir)
import Debian.Repo.Internal.Repos (MonadRepos)
import Debian.Repo.Top (MonadTop)
import Distribution.Compiler (CompilerFlavor)
import System.FilePath ((</>))

-- | Given a RetrieveMethod, perform the retrieval and return the result.
retrieve :: forall m. (MonadOS m, MonadRepos m, MonadTop m, MonadCatch m) =>
            CompilerFlavor -> DebT IO () -> C.CacheRec -> P.Packages -> m Download
retrieve hc defaultAtoms cache target =
     case P.spec target of
      P.Apt dist package -> Apt.prepare cache target dist (SrcPkgName package)
      P.Bzr string -> Bzr.prepare cache target string

      P.Cd dir spec' ->
          retrieve hc defaultAtoms cache (target {P.spec = spec'}) >>= \ target' ->
          return $ Download { T.package = target
                            , T.getTop = getTop target' </> dir
                            , T.logText = logText target' ++ " (in subdirectory " ++ dir ++ ")"
                            , T.mVersion = Nothing
                            , T.origTarball = Nothing
                            , T.cleanTarget = cleanTarget target'
                            , T.buildWrapper = id
                            }

      P.Darcs uri -> Darcs.prepare cache target uri

      P.DataFiles base files loc ->
          do base' <- retrieve hc defaultAtoms cache (target {P.spec = base})
             files' <- retrieve hc defaultAtoms cache (target {P.spec = files})
             baseTree <- liftIO (findSourceTree (T.getTop base') :: IO SourceTree)
             filesTree <- liftIO (findSourceTree (T.getTop files') :: IO SourceTree)
             _ <- liftIO $ copySourceTree filesTree (dir' baseTree </> loc)
             return base'

      P.DebDir upstream debian ->
          do upstream' <- retrieve hc defaultAtoms cache (target {P.spec = upstream})
             debian' <- retrieve hc defaultAtoms cache (target {P.spec = debian})
             DebDir.prepare target upstream' debian'
      P.Debianize package ->
          retrieve hc defaultAtoms cache (target {P.spec = package}) >>=
          Debianize.prepare hc defaultAtoms cache target

      -- Dir is a simple instance of BuildTarget representing building the
      -- debian source in a local directory.  This type of target is used
      -- for testing, and is also returned by the clean method when the
      -- source control information has been stripped out of some other type
      -- of BuildTarget.
      P.Dir path ->
          do tree <- liftIO (findSourceTree path :: IO SourceTree)
             return $ T.Download { T.package = target
                                 , T.getTop = topdir tree
                                 , T.logText =  "Built from local directory " ++ show (P.spec target)
                                 , T.mVersion = Nothing
                                 , T.origTarball = Nothing
                                 , T.cleanTarget = \ _ -> return ([], 0)
                                 , T.buildWrapper = id
                                 }

      P.Git uri specs -> Git.prepare cache target uri specs
      P.Hackage package -> Hackage.prepare cache target package
      P.Hg string -> Hg.prepare cache target string
      P.Patch base patch ->
          retrieve hc defaultAtoms cache (target {P.spec = base}) >>=
          Patch.prepare target patch

      P.Proc spec' ->
          retrieve hc defaultAtoms cache (target {P.spec = spec'}) >>= \ base ->
          return $ T.Download {
                       T.package = target
                     , T.getTop = T.getTop base
                     , T.logText = T.logText base ++ " (with /proc mounted)"
                     , T.mVersion = Nothing
                     , T.origTarball = Nothing
                     , T.cleanTarget = T.cleanTarget base
                     , T.buildWrapper = withProc
                     }
      P.Quilt base patches ->
          do base' <- retrieve hc defaultAtoms cache (target {P.spec = base})
             patches' <- retrieve hc defaultAtoms cache (target {P.spec = patches})
             Quilt.prepare target base' patches'
      P.SourceDeb spec' ->
          retrieve hc defaultAtoms cache (target {P.spec = spec'}) >>=
          SourceDeb.prepare cache target
      P.Svn uri -> Svn.prepare cache target uri
      P.Tla string -> Tla.prepare cache target string
      P.Twice base -> retrieve hc defaultAtoms cache (target {P.spec = base}) >>=
                      Twice.prepare target
      P.Uri uri sum -> Uri.prepare cache target uri sum

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
