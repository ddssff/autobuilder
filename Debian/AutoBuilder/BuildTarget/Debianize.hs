{-# LANGUAGE PackageImports, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
-- |The intent is that this target debianize any cabal target, but currently
-- it combines debianization with the hackage target.
module Debian.AutoBuilder.BuildTarget.Debianize
    ( prepare
    , documentation
    ) where

import Control.Lens (set, (.=))
import Control.Monad (when)
import Control.Monad.State (evalStateT, modify)
import Control.Monad.Catch (MonadMask, bracket)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Either (partitionEithers)
import Data.List (isSuffixOf, intercalate, partition)
import Data.Maybe (mapMaybe)
import Data.Monoid ((<>))
import Data.Version (Version)
import Debian.AutoBuilder.BuildEnv (envSet)
import Debian.AutoBuilder.Params (computeTopDir)
import qualified Debian.AutoBuilder.Types.CacheRec as P (CacheRec(params))
import qualified Debian.AutoBuilder.Types.Download as T
import qualified Debian.AutoBuilder.Types.Packages as P
import qualified Debian.AutoBuilder.Types.ParamRec as P (buildRelease)
import Debian.Debianize as Cabal hiding (package) -- (CabalT, compileArgs, debianize, evalCabalT, makeAtoms, runDebianizeScript, SourceFormat(Native3), sourceFormat, sourcePackageName, writeDebianization, (~?=), debInfo)
import Debian.Debianize.Optparse (handleBehaviorAdjustment, parseProgramArguments, parseProgramArguments', CommandLineOptions(..))
--import Debian.Debianize.InputCabal (dependOS, newFlags, buildEnv)
--import Debian.Debianize.Monad (liftCabal)
--import Debian.Debianize.Types.Atoms (newAtoms)
import Debian.Pretty (ppShow)
import Debian.Relation (SrcPkgName(..))
import Debian.Repo.Fingerprint (RetrieveMethod(Debianize''), retrieveMethodMD5)
import Debian.Repo.Internal.Repos (MonadRepos)
import Debian.Repo.Prelude.Verbosity (qPutStrLn)
import Debian.Repo.Rsync (rsyncOld)
import Debian.Repo.Top (MonadTop, sub, runTopT)
import Distribution.Verbosity (normal)
import Distribution.Package (PackageIdentifier(..))
import Distribution.PackageDescription (GenericPackageDescription(..), PackageDescription(..))
import Distribution.PackageDescription.Parse (readPackageDescription)
import System.Directory (getDirectoryContents, createDirectoryIfMissing, getCurrentDirectory, setCurrentDirectory)
import System.Environment (withArgs)
import System.FilePath ((</>), takeDirectory)
import System.Process (showCommandForUser)
import System.Unix.Directory (removeRecursiveSafely)
import System.Unix.Mount (withProcAndSys)

documentation :: [String]
documentation = [ "hackage:<name> or hackage:<name>=<version> - a target of this form"
                , "retrieves source code from http://hackage.haskell.org." ]

data T.Download a => DebianizeDL a
    = DebianizeDL { def :: CabalT IO ()
                  , method :: RetrieveMethod
                  , debFlags :: [P.PackageFlag]
                  , cabal :: a
                  , version :: Version
                  , dir :: FilePath }

instance T.Download a => T.Download (DebianizeDL a) where
    method = method
    flags = debFlags
    getTop = dir
    logText x = "Built from hackage, revision: " ++ show (method x)
    mVersion = Just . version
    origTarball = T.origTarball . cabal
    flushSource x = T.flushSource (cabal x) >> liftIO (removeRecursiveSafely (dir x))
    cleanTarget x = \ top -> T.cleanTarget (cabal x) top
    attrs = T.attrs . cabal

-- | Debianize the download, which is assumed to be a cabal package.
prepare :: (MonadRepos m, MonadTop m, T.Download a) => CabalT IO () -> P.CacheRec -> RetrieveMethod -> [P.PackageFlag] -> a -> m T.SomeDownload
prepare defaultAtoms cache method@(Debian.Repo.Fingerprint.Debianize'' _ sourceName) flags cabal =
    do let cabdir = T.getTop cabal
       debdir <- sub ("debianize" </> retrieveMethodMD5 method)
       liftIO $ createDirectoryIfMissing True debdir
       _ <- rsyncOld [] cabdir debdir
       cabfiles <- liftIO $ getDirectoryContents cabdir >>= return . filter (isSuffixOf ".cabal")
       case cabfiles of
         [cabfile] ->
             do desc <- liftIO $ readPackageDescription normal (cabdir </> cabfile)
                -- let (PackageName name) = pkgName . package . packageDescription $ desc
                let version = pkgVersion . package . Distribution.PackageDescription.packageDescription $ desc
                -- We want to see the original changelog, so don't remove this
                -- removeRecursiveSafely (dir </> "debian")
                liftIO $ autobuilderCabal cache flags sourceName debdir defaultAtoms
                return $ T.SomeDownload $ DebianizeDL { def = defaultAtoms
                                                      , method = method
                                                      , debFlags = flags
                                                      , cabal = cabal
                                                      , version = version
                                                      , dir = debdir }
         _ -> error $ "Download at " ++ cabdir ++ ": missing or multiple cabal files"
prepare _ _ method _ _ = error $ "Unexpected method passed to Debianize.prepare: " ++ show method

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

-- | Run the autobuilder's version of the cabal-debian program.  First
-- it looks for a debian/Debianize.hs script and tries to run that, if
-- that doesn't work it runs cabal-debian --native, adding any options
-- it can infer from the package flags.
autobuilderCabal :: P.CacheRec -> [P.PackageFlag] -> Maybe String -> FilePath -> CabalT IO () -> IO ()
autobuilderCabal cache pflags sourceName debianizeDirectory defaultAtoms =
    Cabal.withCurrentDirectory debianizeDirectory $
    do let rel = P.buildRelease $ P.params cache
       top <- computeTopDir (P.params cache)
       eset <- runTopT top (envSet rel)
       let (functions, flags) = partitionEithers (map (\ x -> case x of P.ModifyAtoms fn -> Left fn; _ -> Right x) pflags)
       v <- return 0 -- verbosity
       let args =
               groom $
                   ["--native"] ++
                   maybe [] (\ name -> ["--source-package-name", name]) sourceName ++
                   ["--buildenvdir", takeDirectory (dependOS eset)] ++
                   replicate v "--verbose" ++
                   concatMap asCabalFlags flags
       -- This will be false if the package has no debian/Debianize.hs script
       done <- runDebianizeScript args
       when (not done) $ do
             withArgs args $
                do (opts :: CommandLineOptions) <- parseProgramArguments' args
                   -- (moreOpts :: [CommandLineOptions]) <- mapM parseProgramArguments' (map asCabalFlags pflags)
                   st <- newCabalInfo (_flags opts)
                   evalCabalT (do -- We don't actually run the cabal-debian command here, we use
                                  -- the library API and build and print the equivalent command.
                                  qPutStrLn (" -> cabal-debian " <> intercalate " " args ++ " (in " ++ debianizeDirectory ++ ")")
                                  handleBehaviorAdjustment (_adjustment opts)
                                  modify (foldl (.) id functions)
                                  (debInfo . sourceFormat) .= Native3
                                  (debInfo . sourcePackageName) .?= fmap SrcPkgName sourceName
                                  Cabal.debianize (defaultAtoms >> mapM_ modify functions)
                                  liftCabal writeDebianization) st

groom args = foldl nubOpt args ["--disable-tests", "--no-tests"]
    where
      nubOpt args opt = case partition (== opt) args of
                          ([], _) -> args
                          (_, args) -> args ++ [opt]

class CabalFlags a where
    asCabalFlags :: a -> [String]

instance CabalFlags P.PackageFlag where
    asCabalFlags (P.Maintainer s) = ["--maintainer", s]
    asCabalFlags (P.BuildDep s) = ["--build-dep", s]
    asCabalFlags (P.DevelDep s) = ["--build-dep", s, "--dev-dep", s]
    asCabalFlags (P.MapDep c d) = ["--dep-map", c ++ ":" ++ ppShow d]
    asCabalFlags (P.DebVersion s) = ["--deb-version", s]
    asCabalFlags (P.SkipVersion _) = []
    asCabalFlags (P.FailVersion _) = []
    asCabalFlags P.SkipPackage = []
    asCabalFlags P.FailPackage = []
    asCabalFlags (P.Revision s) = ["--revision", s]
    asCabalFlags (P.Epoch name d) = ["--epoch-map", name ++ "=" ++ show d]
    asCabalFlags P.NoDoc = ["--disable-haddock"]
    asCabalFlags P.NoHoogle = ["--no-hoogle"]
    -- P.CabalDebian is the most future proof way to pass options to
    -- cabal-debian, most of the other cases can be done with this one
    asCabalFlags (P.CabalDebian ss) = ss
    asCabalFlags (P.RelaxDep _) = []
    asCabalFlags (P.UDeb _) = []
    asCabalFlags P.OmitLTDeps = [] -- I think this exists
    asCabalFlags (P.AptPin _) = []
    asCabalFlags (P.CabalPin _) = []
    asCabalFlags (P.ModifyAtoms _) = []
    asCabalFlags (P.DarcsTag _) = []
    asCabalFlags (P.GitBranch _) = []
    asCabalFlags P.KeepRCS = []
