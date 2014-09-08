{-# Language CPP, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables #-}
module Debian.AutoBuilder.Types.Buildable
    ( failing
    , Buildable(..)
    , asBuildable
    , relaxDepends
    , Target(Target, tgt, cleanSource, targetDepends)
    , prepareTarget
    , targetRelaxed
    , getRelaxedDependencyInfo
    ) where

import Control.Applicative ((<$>))
import Control.Applicative.Error (Failing(Success, Failure), ErrorMsg)
import Control.Exception as E (SomeException, try, catch, throw)
import Control.Monad(when)
import Control.Monad.Trans (MonadIO, liftIO)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Debian.AutoBuilder.Types.CacheRec as C
import qualified Debian.AutoBuilder.Types.Download as T
import Debian.AutoBuilder.Types.Download (Download(..))
import qualified Debian.AutoBuilder.Types.Packages as P
import qualified Debian.AutoBuilder.Types.ParamRec as R
import Debian.AutoBuilder.Types.Packages (foldPackages)
import Debian.AutoBuilder.Types.ParamRec (ParamRec(..))
import Debian.Changes (logVersion, ChangeLogEntry(..))
import Debian.Control (HasDebianControl(debianControl))
import Debian.Control.Policy (debianSourcePackageName, parseDebianControlFromFile)
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

{- -- This is now in the Triplets module of cabal-debian
instance Monad Failing where
  return = Success
  m >>= f =
      case m of
        (Failure errs) -> (Failure errs)
        (Success a) -> f a
  fail errMsg = Failure [errMsg]
-}

-- | A replacement for the BuildTarget class and the BuildTarget.* types.  The method code
-- moves into the function that turns a RetrieveMethod into a BuildTarget.
data Buildable
    = Buildable
      { download :: Download
      , debianSourceTree :: DebianSourceTree
      -- ^ Return the debian source tree.  Every target must have
      -- this, since this program only builds debian packages.
      }

instance HasDebianControl Buildable where
    debianControl = control' . debianSourceTree

-- | Try to turn a Download into a Target.  First look for a debianization in the
-- top directory, then for debianizations in subdirectory.  This will throw an
-- exception if we can't find any, or we find too many.
asBuildable :: Download -> IO Buildable
asBuildable x =
    try (findSourceTree (getTop x)) >>=
            either (\ (_ :: SomeException) ->
                        -- qPutStrLn ("No source tree found in " ++ getTop x ++ " (" ++ show e ++ ")") >>
                        findDebianBuildTrees (getTop x) >>= \ trees ->
                        case trees of
                          [tree] -> return (Buildable { download = x, debianSourceTree = debTree' tree})
                          [] -> error $ "No build trees found in " ++ getTop x
                          _ -> error $ "Multiple build trees found in " ++ getTop x)
                   (\ tree -> return (Buildable { download = x, debianSourceTree = tree}))

-- | Prevent the appearance of a new binary package from
-- triggering builds of its build dependencies.  Optionally, a
-- particular source package can be specified whose rebuild will
-- be prevented.  This is used to break dependency loops, For
-- example, @Relax-Depends: ghc6 hscolour@ means \"even if ghc6
-- is rebuilt, don't rebuild hscolour even though ghc6 is one of
-- its build dependencies.\"
relaxDepends :: C.CacheRec -> Buildable -> G.OldRelaxInfo
relaxDepends cache@(C.CacheRec {C.params = p}) x =
    let srcPkg = debianSourcePackageName x in
    G.RelaxInfo $ map (\ target -> (BinPkgName target, Nothing)) (globalRelaxInfo (C.params cache)) ++
                  foldPackages (\ _spec flags xs -> xs ++ map (\ binPkg -> (BinPkgName binPkg, Just srcPkg)) (P.relaxInfo flags)) (R.buildPackages p) []

_makeRelaxInfo :: G.OldRelaxInfo -> G.RelaxInfo
_makeRelaxInfo (G.RelaxInfo xs) srcPkgName binPkgName =
    Set.member binPkgName global || maybe False (Set.member binPkgName) (Map.lookup srcPkgName mp)
    where
      (global :: Set.Set BinPkgName, mp :: Map.Map SrcPkgName (Set.Set BinPkgName)) =
          foldr f (Set.empty, Map.empty) xs
      f (b, Just s) (global', mp') = (global', Map.insertWith Set.union s (Set.singleton b) mp')
      f (b, Nothing) (global', mp') = (Set.insert b global', mp')

-- | Information collected from the build tree for a Tgt.
data Target
    = Target { tgt :: Buildable			-- ^ The instance of BuildTarget
             , cleanSource :: DebianBuildTree	-- ^ The source code stripped of SCCS info
             , targetDepends :: G.DepInfo	-- ^ The dependency info parsed from the control file
             }

instance HasDebianControl Target where
    debianControl = debianControl . tgt

instance Eq Target where
    a == b = debianSourcePackageName a == debianSourcePackageName b

-- |Prepare a target for building in the given environment.  At this
-- point, the target needs to be a DebianSourceTree or a
-- DebianBuildTree.
prepareTarget :: (MonadOS m, MonadIO m) => C.CacheRec -> Relations -> Buildable -> m Target
prepareTarget cache globalBuildDeps source =
    prepareBuild cache (download source) >>= \ tree ->
    liftIO (getTargetDependencyInfo globalBuildDeps tree) >>= \ deps ->
    return $ Target { tgt = source, cleanSource = tree, targetDepends = deps }

-- | Given a download, examine it to see if it is a debian source
-- tree, and if that fails see if it is a debian build tree.  Copy the
-- result into the build position of the OSImage, and clean out any
-- revision control files.  This ensures that the tarball and\/or the
-- .diff.gz file in the deb don't contain extra junk.  It also makes
-- sure that debian\/rules is executable.
prepareBuild :: (MonadOS m, MonadIO m) => C.CacheRec -> T.Download -> m DebianBuildTree
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
{-    checkName tree = source == Just name
          where source = fieldValue "Source" (head (unControl (control' (debTree' tree)))) -}

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

{-
updateDependencyInfo :: G.RelaxInfo -> Relations -> [Target] -> IO [Target]
updateDependencyInfo relaxInfo globalBuildDeps targets =
    q12 "Updating dependency info" $
    getDependencyInfo globalBuildDeps targets >>=
    (\ ts -> qPutStrLn ("Original dependencies: " ++ show (map (prettyTarget . targetDepends) ts)) >> return ts) >>=
    (\ ts -> qPutStrLn ("Relaxed dependencies:  " ++ show (map (prettyTarget . targetRelaxed relaxInfo) ts)) >> return ts)
-}
{-
prettyTarget :: (G.SrcPkgName, Relations, [G.BinPkgName]) -> Doc
prettyTarget (src, relss, _bins) = cat (intersperse (text ", ")  (map (prettyRels src) relss))

prettyRels :: G.SrcPkgName -> [Relation] -> Doc
prettyRels src rels =             cat (intersperse (text " | ") (map (\ rel -> cat [prettySrcPkgName src, prettyRelation rel]) rels))

prettySrcPkgName :: G.SrcPkgName -> Doc
prettySrcPkgName (G.SrcPkgName pkgname) = text pkgname
-}
{-
prettyPkgVersion :: PkgVersion -> Doc
prettyPkgVersion v = cat [text (getName v ++ "-"), prettyVersion (getVersion v)]

instance Show Target where
    show target = show . tgt $ target
-}

{-
-- |Retrieve the dependency information for a list of targets
getDependencyInfo :: Relations -> [Target] -> IO [Target]
getDependencyInfo globalBuildDeps targets =
    mapM (getTargetDependencyInfo globalBuildDeps) (cleanSource targets) >>=
    finish . partitionEithers . zipEithers targets . map eff
    where
      finish ([], ok) = return (map (\ (target, deps) -> target {targetDepends = deps}) ok)
      finish (bad, ok) =
          do -- FIXME: Any errors here should be fatal
             qPutStrLn ("Unable to retrieve build dependency info for some targets:\n  " ++
                           concat (intersperse "\n  " (map (\ (target, message) -> targetName target ++ ": " ++ message) bad)))
             return (map (\ (target, deps) -> target {targetDepends = deps}) ok)
-}

getRelaxedDependencyInfo :: Relations -> G.OldRelaxInfo -> DebianBuildTree -> IO (G.DepInfo, G.DepInfo)
getRelaxedDependencyInfo globalBuildDeps relaxInfo tree =
    do deps <- getTargetDependencyInfo globalBuildDeps tree
       return (deps, head (G.oldRelaxDeps relaxInfo [deps]))

targetRelaxed :: G.OldRelaxInfo -> Target -> G.DepInfo
targetRelaxed relaxInfo target = head $ G.oldRelaxDeps relaxInfo [targetDepends target]

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

{-
zipEithers :: [a] -> [Either b c] -> [Either (a, b) (a, c)]
zipEithers xs ys = 
    map zipEither (zip xs ys)
    where
      zipEither :: (a, Either b c) -> Either (a, b) (a, c)
      zipEither (x, (Left y)) = Left (x, y)
      zipEither (x, (Right y)) = Right (x, y)

-- Failing From Either - use during conversion
-- ffe :: IO a -> IO (Failing a)
-- ffe a = try a >>= return . either (\ (e :: SomeException) -> Failure [show e]) Success

eff (Failure ss) = Left (intercalate "\n" ss)
eff (Success x) = Right x
-}
