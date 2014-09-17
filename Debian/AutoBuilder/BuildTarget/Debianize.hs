{-# LANGUAGE PackageImports, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
-- |The intent is that this target debianize any cabal target, but currently
-- it combines debianization with the hackage target.
module Debian.AutoBuilder.BuildTarget.Debianize
    ( prepare
    , documentation
    ) where

import Control.Monad.State (modify)
import Control.Monad.Catch (MonadMask, bracket)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.List (isSuffixOf)
import Debian.AutoBuilder.BuildEnv (envSet)
import Debian.AutoBuilder.Params (computeTopDir)
import qualified Debian.AutoBuilder.Types.CacheRec as P
import qualified Debian.AutoBuilder.Types.Download as T
import qualified Debian.AutoBuilder.Types.Packages as P
import qualified Debian.AutoBuilder.Types.ParamRec as P
import Debian.Debianize as Cabal hiding (verbosity, withCurrentDirectory)
import Debian.Pretty (ppDisplay)
import Debian.Relation (SrcPkgName(..))
import qualified Debian.Repo.Fingerprint as P
import Debian.Repo.Prelude (rsync)
import Debian.Repo.Internal.Repos (MonadRepos)
import Debian.Repo.Top (MonadTop, sub, runTopT)
import Distribution.Verbosity (normal)
import Distribution.Package (PackageIdentifier(..))
import Distribution.PackageDescription (GenericPackageDescription(..), PackageDescription(..))
import Distribution.PackageDescription.Parse (readPackageDescription)
import System.Directory (getDirectoryContents, createDirectoryIfMissing, getCurrentDirectory, setCurrentDirectory)
import System.Environment (withArgs)
import System.FilePath ((</>), takeFileName, takeDirectory)
import System.Process (showCommandForUser)
import Debian.Repo.Prelude.Verbosity (qPutStrLn)

documentation :: [String]
documentation = [ "hackage:<name> or hackage:<name>=<version> - a target of this form"
                , "retrieves source code from http://hackage.haskell.org." ]

-- | Debianize the download, which is assumed to be a cabal package.
prepare :: (MonadRepos m, MonadTop m) => DebT IO () -> P.CacheRec -> P.Packages -> [P.DebSpec] -> T.Download -> m T.Download
prepare defaultAtoms cache package' specs target =
    do dir <- sub ("debianize" </> takeFileName (T.getTop target))
       liftIO $ createDirectoryIfMissing True dir
       _ <- rsync [] (T.getTop target) dir
       cabfiles <- liftIO $ getDirectoryContents dir >>= return . filter (isSuffixOf ".cabal")
       case cabfiles of
         [cabfile] ->
             do desc <- liftIO $ readPackageDescription normal (dir </> cabfile)
                -- let (PackageName name) = pkgName . package . packageDescription $ desc
                let version = pkgVersion . package . Distribution.PackageDescription.packageDescription $ desc
                -- We want to see the original changelog, so don't remove this
                -- removeRecursiveSafely (dir </> "debian")
                liftIO $ autobuilderCabal cache (P.flags package') specs dir defaultAtoms
                return $ T.Download { T.package = package'
                                    , T.getTop = dir
                                    , T.logText =  "Built from hackage, revision: " ++ show (P.spec package')
                                    , T.mVersion = Just version
                                    , T.origTarball = T.origTarball target
                                    , T.cleanTarget = \ top -> T.cleanTarget target top
                                    , T.buildWrapper = id
                                    , T.attrs = T.attrs target }
         _ -> error $ "Download at " ++ dir ++ ": missing or multiple cabal files"

withCurrentDirectory :: (MonadMask m, MonadIO m) => FilePath -> m a -> m a
withCurrentDirectory new action = bracket (liftIO getCurrentDirectory >>= \ old -> liftIO (setCurrentDirectory new) >> return old) (liftIO . setCurrentDirectory) (\ _ -> action)

{-
-- | Run cabal-debian on the given directory, creating or updating the
-- debian subdirectory.  If the script in debian/Debianize.hs fails this
-- will throw an exception.
autobuilderDebianize :: P.CacheRec -> [P.PackageFlag] -> FilePath -> IO ()
autobuilderDebianize cache pflags currentDirectory =
    withCurrentDirectory currentDirectory $
    do args <- collectPackageFlags cache pflags
       done <- Cabal.runDebianize args
       when (not done) (Cabal.callDebianize args)
-}

-- | Convert a set of package flags into the corresponding
-- cabal-debian command line options.  (Is this really in the IO monad
-- for a good reason?)
collectPackageFlags :: P.CacheRec -> [P.PackageFlag] -> IO [String]
collectPackageFlags _cache pflags =
    do v <- return 0 -- verbosity
       return $ ["--verbose=" ++ show v] ++
                concatMap asCabalFlags pflags

autobuilderCabal :: P.CacheRec -> [P.PackageFlag] -> [P.DebSpec] -> FilePath -> DebT IO () -> IO ()
autobuilderCabal cache pflags specs debianizeDirectory defaultAtoms =
    withCurrentDirectory debianizeDirectory $
    do let rel = P.buildRelease $ P.params cache
       top <- computeTopDir (P.params cache)
       eset <- runTopT top (envSet rel)
       args <- collectPackageFlags cache pflags
       let args' = "--buildenvdir" : takeDirectory (dependOS eset) : args
       -- This will be false if the package has no debian/Debianize.hs script
       done <- runDebianizeScript args'
       case done of
         True -> qPutStrLn (showCommandForUser "runhaskell" ("debian/Debianize.hs" : args'))
         False -> withArgs [] $ Cabal.evalDebT (do qPutStrLn (showCommandForUser "cabal-debian" (["--native"] ++ concatMap asCabalFlags specs ++ concatMap asCabalFlags pflags))
                                                   debianization defaultAtoms (applyDebSpecs specs >> mapM_ applyPackageFlag pflags)
                                                   writeDebianization)
                                               (makeAtoms eset)

applyDebSpecs :: [P.DebSpec] -> DebT IO ()
applyDebSpecs specs = mapM_ applyDebSpec specs

applyDebSpec:: P.DebSpec -> DebT IO ()
applyDebSpec = compileArgs . asCabalFlags

applyPackageFlag :: P.PackageFlag -> DebT IO ()
applyPackageFlag (P.ModifyAtoms f) = modify f
applyPackageFlag x = compileArgs . asCabalFlags $ x

class CabalFlags a where
    asCabalFlags :: a -> [String]

instance CabalFlags P.DebSpec where
    asCabalFlags (P.SrcDeb name) = ["--source-package-name", unSrcPkgName name]

instance CabalFlags P.PackageFlag where
    asCabalFlags (P.Maintainer s) = ["--maintainer", s]
    asCabalFlags (P.BuildDep s) = ["--build-dep", s]
    asCabalFlags (P.DevelDep s) = ["--build-dep", s, "--dev-dep", s]
    asCabalFlags (P.MapDep c d) = ["--map-dep", c ++ "=" ++ ppDisplay d]
    asCabalFlags (P.DebVersion s) = ["--deb-version", s]
    asCabalFlags (P.SkipVersion _) = []
    asCabalFlags (P.FailVersion _) = []
    asCabalFlags P.SkipPackage = []
    asCabalFlags P.FailPackage = []
    asCabalFlags (P.Revision s) = ["--revision", s]
    asCabalFlags (P.Epoch name d) = ["--epoch-map", name ++ "=" ++ show d]
    asCabalFlags P.NoDoc = ["--disable-haddock"]
    asCabalFlags P.NoHoogle = ["--no-hoogle"]
    asCabalFlags (P.CabalDebian ss) = ss
    asCabalFlags (P.RelaxDep _) = []
    asCabalFlags (P.UDeb _) = []
    asCabalFlags P.OmitLTDeps = [] -- I think this exists
    asCabalFlags (P.AptPin _) = []
    asCabalFlags (P.CabalPin _) = []
    asCabalFlags (P.ModifyAtoms _) = ["<un-showable-modifyatoms-function>"]
    asCabalFlags (P.DarcsTag _) = []
    asCabalFlags (P.GitBranch _) = []
    asCabalFlags P.KeepRCS = []
