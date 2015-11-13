{-# Language CPP, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables #-}
module Debian.AutoBuilder.Types.Buildable
    ( failing
    , Buildable(..)
    , asBuildable
    , relaxDepends
    , Target(Target, tgt, cleanSource, targetControl)
    , prepareTarget
    , targetRelaxed
    , getRelaxedDependencyInfo
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Control.Applicative.Error (Failing(Success, Failure), ErrorMsg)
import Control.Exception as E (SomeException, try, catch, throw)
import Control.Lens (view)
import Control.Monad (when)
import Control.Monad.Catch (MonadMask)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Map ((!))
import qualified Debian.AutoBuilder.Types.CacheRec as C
import qualified Debian.AutoBuilder.Types.Download as T
import Debian.AutoBuilder.Types.Download (Download(..))
import qualified Debian.AutoBuilder.Types.Packages as P
import qualified Debian.AutoBuilder.Types.ParamRec as R
import Debian.AutoBuilder.Types.Packages (pid)
import Debian.AutoBuilder.Types.ParamRec (ParamRec(..))
import Debian.Changes (logVersion, ChangeLogEntry(..))
import Debian.Control (HasDebianControl(debianControl))
import Debian.Control.Policy (DebianControl, debianSourcePackageName, parseDebianControlFromFile)
import qualified Debian.GenBuildDeps as G
import Debian.Relation (SrcPkgName(..), BinPkgName(..))
import Debian.Relation.ByteString(Relations)
import Debian.Repo.MonadOS (MonadOS(getOS))
import Debian.Repo.OSImage (osRoot)
import Debian.Repo.SourceTree (DebianBuildTree(..), entry, subdir, debdir, findDebianBuildTrees, findBuildTree, copySourceTree,
                               DebianSourceTree(..), findSourceTree)
import Debian.Repo.EnvPath (EnvRoot(rootPath))
import qualified Debian.Version
import System.Directory(renameDirectory)
import System.FilePath (takeExtension, (</>))
import System.IO.Error (isAlreadyExistsError)
import System.Posix.Files (createLink, removeLink)
import Debian.Repo.Prelude.Verbosity (quieter, qPutStrLn)

-- | Case analysis for the 'Failing' type.
-- If the value is @'Failure'@, apply the first function to @[ErrorMsg]@;
-- if it is @'Success' a@, apply the second function to @a@.
failing :: ([ErrorMsg] -> b) -> (a -> b) -> Failing a -> b
failing f _ (Failure errs) = f errs
failing _ f (Success a)    = f a

-- | A replacement for the BuildTarget class and the BuildTarget.* types.  The method code
-- moves into the function that turns a RetrieveMethod into a BuildTarget.
data Buildable download
    = Buildable
      { download :: download
      , debianSourceTree :: DebianSourceTree
      -- ^ Return the debian source tree.  Every target must have
      -- this, since this program only builds debian packages.
      }

instance HasDebianControl (Buildable download) where
    debianControl = control' . debianSourceTree

-- | Try to turn a Download into a Target.  First look for a debianization in the
-- top directory, then for debianizations in subdirectory.  This will throw an
-- exception if we can't find any, or we find too many.
asBuildable :: (T.Download a) => a -> IO (Buildable a)
asBuildable x =
    try (findSourceTree (getTop x)) >>=
            either (\ (_ :: SomeException) ->
                        -- qPutStrLn ("No source tree found in " ++ getTop x ++ " (" ++ show e ++ ")") >>
                        findDebianBuildTrees (getTop x) >>= \ trees ->
                        case trees of
                          [tree] -> return (Buildable { download = x, debianSourceTree = debTree' tree})
                          [] -> error $ "No Debian build trees found in " ++ getTop x
                          _ -> error $ "Multiple Debian build trees found in " ++ getTop x)
                   (\ tree -> return (Buildable { download = x, debianSourceTree = tree}))

-- | Prevent the appearance of a new binary package from
-- triggering builds of its build dependencies.  Optionally, a
-- particular source package can be specified whose rebuild will
-- be prevented.  This is used to break dependency loops, For
-- example, @Relax-Depends: ghc6 hscolour@ means \"even if ghc6
-- is rebuilt, don't rebuild hscolour even though ghc6 is one of
-- its build dependencies.\"
relaxDepends :: (T.Download a) => C.CacheRec -> Buildable a -> SrcPkgName -> BinPkgName -> Bool
relaxDepends c x s b =
    any (== b) (map BinPkgName (globalRelaxInfo (C.params c))) ||
    (debianSourcePackageName x == s &&
     any (== b) (map BinPkgName (concatMap (P.relaxInfo . P._flags . get) (R.buildPackages (C.params c)))))
    where
      get i = R.knownPackages (C.params c) ! i

-- | Information collected from the build tree for a Tgt.
data Target download
    = Target { tgt :: Buildable download        -- ^ The instance of BuildTarget
             , cleanSource :: DebianBuildTree   -- ^ The source code stripped of SCCS info
             , targetControl :: DebianControl   -- ^ The dependency control file
             }

instance HasDebianControl (Target download) where
    debianControl = debianControl . tgt

instance Eq (Target download) where
    a == b = debianSourcePackageName a == debianSourcePackageName b

-- |Prepare a target for building in the given environment.  At this
-- point, the target needs to be a DebianSourceTree or a
-- DebianBuildTree.
prepareTarget :: (MonadOS m, MonadIO m, MonadMask m, T.Download a) => C.CacheRec -> Buildable a -> m (Target a)
prepareTarget cache source =
    prepareBuild cache (download source) >>= \ tree ->
    liftIO (getTargetControlInfo tree) >>= \ ctl ->
    return $ Target { tgt = source, cleanSource = tree, targetControl = ctl }

-- | Given a download, examine it to see if it is a debian source
-- tree, and if that fails see if it is a debian build tree.  Copy the
-- result into the build position of the OSImage, and clean out any
-- revision control files.  This ensures that the tarball and\/or the
-- .diff.gz file in the deb don't contain extra junk.  It also makes
-- sure that debian\/rules is executable.
prepareBuild :: (MonadOS m, MonadIO m, MonadMask m, T.Download a) => C.CacheRec -> a -> m DebianBuildTree
prepareBuild _cache target =
    liftIO (try (findSourceTree (T.getTop target))) >>=
    either (\ (_ :: SomeException) ->
                quieter 1 (qPutStrLn ("Failed to find source tree in " ++ T.getTop target ++ ", trying build trees.")) >>
                liftIO (findDebianBuildTrees (T.getTop target)) >>= \ trees ->
                    case trees of
                      [tree] ->
                          quieter 1 (qPutStrLn ("Found build tree in " ++ topdir' tree)) >>
                          copyBuild tree
                      [] -> error $ "No debian source tree found in " ++ T.getTop target
                      _ -> error $ "Multiple debian source trees found in " ++ T.getTop target)
           copySource
    where
      copySource :: (MonadOS m, MonadIO m) => DebianSourceTree -> m DebianBuildTree
      copySource debSource =
          do root <- rootPath . osRoot <$> getOS
             let name = logPackage . entry $ debSource
                 dest = root ++ "/work/build/" ++ name
                 ver = Debian.Version.version . logVersion . entry $ debSource
                 newdir = escapeForBuild $ name ++ "-" ++ ver
             -- ePutStrLn ("copySource " ++ dir' (tree' debSource) ++ " -> " ++ dest ++ ", tarball=" ++ show (T.origTarball target))
             _copy <- liftIO $ copySourceTree debSource (dest </> newdir)
             -- Clean the revision control files for this target out of the copy of the source tree
             (_out, _time) <- liftIO $ T.cleanTarget target (dest </> newdir)
             maybe (return ()) (liftIO . copyOrigTarball dest name ver) (T.origTarball target)
             liftIO $ findBuildTree dest newdir

      copyBuild :: (MonadOS m, MonadIO m) => DebianBuildTree -> m DebianBuildTree
      copyBuild debBuild =
          do root <- rootPath . osRoot <$> getOS
             let name = logPackage . entry $ debBuild
                 dest = root ++ "/work/build/" ++ name
                 ver = Debian.Version.version . logVersion . entry $ debBuild
                 newdir = escapeForBuild $ name ++ "-" ++ ver
             -- ePutStrLn ("copyBuild " ++ topdir' debBuild ++ " -> " ++ dest ++ ", tarball=" ++ show (T.origTarball target))
             _copy <- liftIO $ copySourceTree debBuild dest
             (_output, _time) <- liftIO $ T.cleanTarget target (dest </> newdir)
             when (newdir /= (subdir debBuild))
                      (liftIO $ renameDirectory (dest ++ "/" ++ subdir debBuild) (dest ++ "/" ++ newdir))
             liftIO $ findBuildTree dest newdir

      copyOrigTarball dest name ver src = forceLink src (dest ++ "/" ++ name ++ "_" ++ ver ++ ".orig.tar" ++ takeExtension src)

-- |calls 'createSymbolicLink' but will remove the target and retry if
-- 'createSymbolicLink' raises EEXIST.
forceLink :: FilePath -> FilePath -> IO ()
forceLink target linkName =
    quieter 1 $ qPutStrLn ("forceLink " ++ target ++ " " ++ linkName) >>
    createLink target linkName `E.catch`
      (\ e -> if isAlreadyExistsError e 
              then do removeLink linkName
                      createLink target linkName
              else throw e)

-- |Make a path "safe" for building.  This shouldn't be necessary,
-- but various packages make various assumptions about the type
-- of characters that appear in the name of the working directory
-- that the build is performed in.  For example, kdenetwork objects
-- to the colon, kdebase objects to the plus sign, and so on.
escapeForBuild :: FilePath -> FilePath
escapeForBuild =
    map escape
    where
      escape ':' = '_'
      escape '+' = '_'
      escape c = c

getRelaxedDependencyInfo :: Relations -> G.RelaxInfo -> DebianBuildTree -> IO (G.DepInfo, G.DepInfo)
getRelaxedDependencyInfo globalBuildDeps relaxInfo tree =
    do deps <- getTargetDependencyInfo globalBuildDeps tree
       return (deps, head (G.relaxDeps relaxInfo [deps]))

targetRelaxed :: (T.Download a) => Relations -> G.RelaxInfo -> Target a -> G.DepInfo
targetRelaxed globalBuildDeps relaxInfo target = head $ G.relaxDeps relaxInfo [targetDepends globalBuildDeps target]

-- |Retrieve the dependency information for a single target
getTargetDependencyInfo :: Relations -> DebianBuildTree -> IO G.DepInfo
getTargetDependencyInfo globalBuildDeps buildTree =
    parseDebianControlFromFile controlPath >>=
    return . either throw G.buildDependencies >>=
    return . addRelations globalBuildDeps
    where
      controlPath = debdir buildTree ++ "/debian/control"
      addRelations :: Relations -> G.DepInfo -> G.DepInfo
      addRelations moreRels info = info {G.relations = G.relations info ++ moreRels}

targetDepends :: Relations -> Target a -> G.DepInfo
targetDepends globalBuildDeps =
    addRelations globalBuildDeps . G.buildDependencies . targetControl
    where
      addRelations :: Relations -> G.DepInfo -> G.DepInfo
      addRelations moreRels info = info {G.relations = G.relations info ++ moreRels}

getTargetControlInfo :: DebianBuildTree -> IO DebianControl
getTargetControlInfo buildTree =
    parseDebianControlFromFile controlPath >>= either throw return
    where
      controlPath = debdir buildTree ++ "/debian/control"
