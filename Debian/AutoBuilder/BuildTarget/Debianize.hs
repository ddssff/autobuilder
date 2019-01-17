{-# LANGUAGE CPP, PackageImports, RecordWildCards, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
-- |The intent is that this target debianize any cabal target, but currently
-- it combines debianization with the hackage target.
module Debian.AutoBuilder.BuildTarget.Debianize
    ( prepare
    , documentation
    ) where

import Control.Lens ((.=))
import Control.Monad (when)
import Control.Monad.Reader (runReaderT)
import Control.Monad.State (modify)
--import Control.Monad.Catch (MonadMask, bracket)
import Control.Monad.Trans (liftIO)
import Data.List (isSuffixOf, partition)
#if !MIN_VERSION_base(4,8,0)
import Data.Monoid ((<>))
#endif
import Debian.AutoBuilder.BuildEnv (envSet)
import Debian.AutoBuilder.Params (computeTopDir)
import qualified Debian.AutoBuilder.Types.CacheRec as CR (CacheRec(params))
import qualified Debian.AutoBuilder.Types.Download as DL
import qualified Debian.AutoBuilder.Types.Packages as PS
import qualified Debian.AutoBuilder.Types.ParamRec as PR (buildRelease)
import Debian.Debianize as Cabal (CabalInfo, withCurrentDirectory, dependOS, performDebianization, (.?=), CabalT, runDebianizeScript, SourceFormat(Native3), sourceFormat, sourcePackageName, debInfo)
import Debian.Pretty (ppShow)
import Debian.Relation (SrcPkgName(..))
import Debian.Repo.Fingerprint (RetrieveMethod(Debianize''), retrieveMethodMD5)
import Debian.Repo.MonadRepos (MonadRepos)
import Debian.Repo.Rsync (rsyncOld)
import Debian.Repo.Top (MonadTop, sub, TopDir(TopDir))
import Distribution.Verbosity (normal)
#if MIN_VERSION_Cabal(2,0,0)
import Distribution.Version (Version)
#else
import Data.Version (Version)
#endif
import Distribution.Package (PackageIdentifier(..))
import Distribution.PackageDescription (GenericPackageDescription(..), PackageDescription(..))
#if MIN_VERSION_Cabal(2,2,0)
import Distribution.PackageDescription.Parsec (readGenericPackageDescription)
#else
import Distribution.PackageDescription.Parse (readPackageDescription)
#endif
import System.Directory (getDirectoryContents, createDirectoryIfMissing)
import System.Environment (withArgs)
import System.FilePath ((</>), takeDirectory)
import System.Unix.Directory (removeRecursiveSafely)

documentation :: [String]
documentation = [ "hackage:<name> or hackage:<name>=<version> - a target of this form"
                , "retrieves source code from http://hackage.haskell.org." ]

data DebianizeDL a
    = DebianizeDL { def :: CabalT IO ()
                  , method :: RetrieveMethod
                  , debFlags :: [PS.PackageFlag]
                  , cabal :: a
                  , version :: Version
                  , dir :: FilePath }

instance Show a => Show (DebianizeDL a) where
    show (DebianizeDL {..}) =
        "DebianizeDL {def = pure (), method = " ++ show method ++ ", debFlags = " ++ show debFlags ++ ", cabal = " ++ show cabal ++ ", version = " ++ show version ++ ", dir = " ++ show dir ++ "}"

instance DL.Download a => DL.Download (DebianizeDL a) where
    method = method
    flags = debFlags
    getTop = dir
    logText x = "Built from hackage, revision: " ++ show (method x)
    mVersion = Just . version
    origTarball = DL.origTarball . cabal
    flushSource x = DL.flushSource (cabal x) >> liftIO (removeRecursiveSafely (dir x))
    cleanTarget x = \ top -> DL.cleanTarget (cabal x) top
    attrs = DL.attrs . cabal

-- | Debianize the download, which is assumed to be a cabal package.
prepare :: (MonadRepos m, MonadTop r m, DL.Download a) => CabalT IO () -> CR.CacheRec -> RetrieveMethod -> [PS.PackageFlag] -> [CabalInfo -> CabalInfo] -> a -> m DL.SomeDownload
prepare defaultAtoms cache method@(Debian.Repo.Fingerprint.Debianize'' _ sourceName) flags functions cabal =
    do let cabdir = DL.getTop cabal
       debdir <- sub ("debianize" </> retrieveMethodMD5 method)
       liftIO $ createDirectoryIfMissing True debdir
       _ <- rsyncOld [] cabdir debdir
       cabfiles <- liftIO $ getDirectoryContents cabdir >>= return . filter (isSuffixOf ".cabal")
       case cabfiles of
         [cabfile] ->
#if MIN_VERSION_Cabal(2,2,0)
             do desc <- liftIO $ readGenericPackageDescription normal (cabdir </> cabfile)
#else
             do desc <- liftIO $ readPackageDescription normal (cabdir </> cabfile)
#endif
                -- let (PackageName name) = pkgName . package . packageDescription $ desc
                let version = pkgVersion . package . Distribution.PackageDescription.packageDescription $ desc
                -- We want to see the original changelog, so don't remove this
                -- removeRecursiveSafely (dir </> "debian")
                liftIO $ autobuilderCabal cache flags functions sourceName debdir defaultAtoms
                return $ DL.SomeDownload $ DebianizeDL { def = defaultAtoms
                                                      , method = method
                                                      , debFlags = flags
                                                      , cabal = cabal
                                                      , version = version
                                                      , dir = debdir }
         _ -> error $ "Download at " ++ cabdir ++ ": missing or multiple cabal files"
prepare _ _ method _ _ _ = error $ "Unexpected method passed to Debianize.prepare: " ++ show method

-- withCurrentDirectory :: (MonadMask m, MonadIO m) => FilePath -> m a -> m a
-- withCurrentDirectory new action = bracket (liftIO getCurrentDirectory >>= \ old -> liftIO (setCurrentDirectory new) >> return old) (liftIO . setCurrentDirectory) (\ _ -> action)

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

-- | Run the autobuilder's version of the cabal-debian program.  First
-- it looks for a debian/Debianize.hs script and tries to run that, if
-- that doesn't work it runs cabal-debian --native, adding any options
-- it can infer from the package flags.
autobuilderCabal :: CR.CacheRec -> [PS.PackageFlag] -> [CabalInfo -> CabalInfo] -> Maybe String -> FilePath -> CabalT IO () -> IO ()
autobuilderCabal cache flags functions sourceName debianizeDirectory defaultAtoms =
    Cabal.withCurrentDirectory debianizeDirectory $
    do let rel = PR.buildRelease $ CR.params cache
       top <- computeTopDir (CR.params cache)
       eset <- runReaderT (envSet rel) (TopDir top)
       -- let (functions, flags) = partitionEithers (map (\ x -> case x of P.ModifyAtoms fn -> Left fn; _ -> Right x) pflags)
       v <- return 0 -- verbosity
       let args =
               groom $
                   ["--native"] ++
                   maybe [] (\ name -> ["--source-package-name", name]) sourceName ++
                   ["--buildenvdir", takeDirectory (dependOS eset)] ++
                   replicate v "-v" ++
                   concatMap asCabalFlags flags
       -- This will return false if the package has no debian/Debianize.hs script.
       -- Note that this will use the *installed* cabal-debian library, if you are
       -- trying to test changes to cabal-debian you need to build and install with
       -- the current source:
       -- ghc debian/Debianize.hs -i/home/dsf/git/cabal-debian/src:/home/dsf/git/autobuilder-seereason '-DMIN_VERSION_Cabal(a,b,c)=1' -o setup
       done <- runDebianizeScript args
       when (not done) $ do
             withArgs args $
                do performDebianization $ do
                     defaultAtoms
                     modify (foldl (.) id functions)
                     (debInfo . sourceFormat) .= Native3
                     (debInfo . sourcePackageName) .?= fmap SrcPkgName sourceName

groom args = foldl nubOpt args ["--disable-tests", "--no-tests"]
    where
      nubOpt args opt = case partition (== opt) args of
                          ([], _) -> args
                          (_, args) -> args ++ [opt]

class CabalFlags a where
    asCabalFlags :: a -> [String]

instance CabalFlags PS.PackageFlag where
    asCabalFlags (PS.Maintainer s) = ["--maintainer", s]
    asCabalFlags (PS.BuildDep s) = ["--build-dep", s]
    asCabalFlags (PS.DevelDep s) = ["--build-dep", s, "--dev-dep", s]
    asCabalFlags (PS.SetupDep _) = []
    asCabalFlags (PS.MapDep c d) = ["--dep-map", c ++ ":" ++ ppShow d]
    asCabalFlags (PS.DebVersion s) = ["--deb-version", s]
    asCabalFlags (PS.SkipVersion _) = []
    asCabalFlags (PS.FailVersion _) = []
    asCabalFlags PS.SkipPackage = []
    asCabalFlags PS.FailPackage = []
    asCabalFlags (PS.Revision s) = ["--revision", s]
    asCabalFlags (PS.Epoch name d) = ["--epoch-map", name ++ "=" ++ show d]
    asCabalFlags PS.NoDoc = ["--disable-haddock"]
    asCabalFlags PS.NoHoogle = ["--no-hoogle"]
    -- PS.CabalDebian is the most future proof way to pass options to
    -- cabal-debian, most of the other cases can be done with this one
    asCabalFlags (PS.CabalDebian ss) = ss
    asCabalFlags (PS.RelaxDep _) = []
    asCabalFlags (PS.UDeb _) = []
    asCabalFlags PS.OmitLTDeps = [] -- I think this exists
    asCabalFlags (PS.AptPin _) = []
    asCabalFlags (PS.CabalPin _) = []
    asCabalFlags (PS.DarcsTag _) = []
    asCabalFlags PS.KeepRCS = []
    asCabalFlags (PS.InGroup _) = []
