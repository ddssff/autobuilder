{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts, FlexibleInstances, RankNTypes, ScopedTypeVariables, TemplateHaskell #-}
-- | The Packages type specifies how to obtain the source code for one
-- or more packages.
module Debian.AutoBuilder.Types.Packages
    ( Package(..), pid, post, spec, flags
    , PackageId
    , GroupName(..)
    , TSt
    , TargetState, release, deps, nodes, packageMap, groups
    , inGroup, inGroups
    , targetState
    , depends
    , PackageFlag(..)
    , createPackage
    , modifyPackage
    , clonePackage
    , plist
    , relaxInfo
    , hackage
    , method
    , deletePackage
    , darcs
    , apt
    , debianize
    , flag
    , mflag
    , apply
    , patch
    , bzr
    , datafiles, datafiles'
    , debdir
    , dir
    , git
    , hg
    , proc
    , quilt
    , sourceDeb
    , svn
    , tla
    , twice
    , uri
    , cd
    , findSource
    , isKeepRCS
    , cabalPin
    , darcsTag
    , aptPin
    , newId
    ) where

import Debug.Trace as D

import Control.Exception (SomeException, try)
import Control.Lens -- (makeLenses, over, set, use, view, (%=))
import Control.Monad.State (StateT)
import Data.ByteString (ByteString)
import Data.Function (on)
import Data.Generics (Data, Typeable)
import Data.Graph.Inductive (Node, LNode, insEdge, insNode, lab, mkGraph, reachable)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Map.Strict as Map (delete, insert, lookup, Map)
import Data.Maybe (fromJust)
import Data.Set as Set (fromList, insert, Set, singleton, toList, unions)
import Data.String (IsString(fromString))
import Debian.Debianize (CabalInfo)
import Debian.Relation as Debian (Relations)
import Debian.Releases (ReleaseTree(..))
import Debian.Repo (DebianSourceTree, findDebianSourceTrees)
import Debian.Repo.Fingerprint (RetrieveMethod(..), GitSpec(..))
import System.FilePath ((</>))
import Text.Show.Functions ()

-- | A type for the group name of a Packages record, used to reference
-- a group of packages.
data GroupName
    = GroupName {unGroupName :: String}
    | NoName
    deriving (Eq, Ord, Show, Data, Typeable)

instance IsString GroupName where
    fromString = GroupName

newtype PackageId = PackageId Int deriving (Eq, Ord, Read, Show, Data, Typeable)

instance Enum PackageId where
    toEnum = PackageId
    fromEnum (PackageId n) = n
    succ (PackageId n) = PackageId (succ n)

-- | Collected information about a package.
data Package
    = Package
      { _pid :: PackageId
      , _spec :: RetrieveMethod
      -- ^ This value describes the method used to download the
      -- package's source code.
      , _flags :: [PackageFlag]
      -- ^ These flags provide additional details about how to obtain
      -- the package.
      , _post :: [CabalInfo -> CabalInfo]
      -- ^ Final transformations to perform on the package info.
      , _groups :: Set GroupName
      } deriving (Show, Data, Typeable) -- We can't derive Eq because post contains functions

type NodeLabel = PackageId
type EdgeLabel = ()

data TargetState
    = TargetState
      { _home :: FilePath    -- ^ typically $HOME, where we find .autobuilder/
      , _release :: ReleaseTree -- ^ e.g. trusty, trusty-seereason
      , _nodes :: Map NodeLabel Node
      , _next :: Node
      , _deps :: Gr NodeLabel EdgeLabel
      , _nextPackageId :: PackageId
      -- , _groups :: Map GroupName (Set PackageId)
      , _packageMap :: Map PackageId Package
      }

type TSt m = StateT TargetState m

-- | Hints about debianizing and building the package.
data PackageFlag
    = RelaxDep String
    -- ^ Build dependencies which should be ignored when deciding whether to rebuild
    | UDeb String
    -- ^ Tell the autobuilder that a binary package name is a udeb.  This means that
    -- we can ignore the package when we are deciding whether we need to do an arch
    -- only build.
    | Maintainer String
    -- ^ Use the given string as maintainer name and email
    | OmitLTDeps
    -- ^ Ignore the << (less than) part of a version dependency when
    -- converting cabal wildcard dependencies.  These can lead to
    -- complex relationships that can't be translated into debian
    -- dependency.
    | AptPin String
    -- ^ Specify the exact debian version of a package to retrieve via apt-get
    | CabalPin String
    -- ^ Specify the exact version of the Cabal package to download from Hackage.
    | BuildDep String
    -- ^ Build dependencies which should be added to the
    -- debian/control file via the --build-dep flag of cabal-debian.
    -- (Formerly ExtraDep.)
    | SetupDep String
    -- ^ A package required to run this package's setup script.
    | DevelDep String
    -- ^ Packages which should be added to the Depends entry for the
    -- dev package in the debian/control file via the --dev-dep flag
    -- of cabal-debian.  Used, for example, to make libssl-dev a
    -- dependency of libghc-hsopenssl-dev.  Implies BuildDep.
    -- (Formerly ExtraDevDep.)
    | NoDoc
    -- ^ Omit the -doc section from the control file so that no
    -- documentation files are generated.  Used to work around haddock
    -- bugs.
    | NoHoogle
    -- ^ Pass the --no-hoogle flag to cabal-debian.
    | CabalDebian [String]
    -- ^ Pass some arbitrary arguments to cabal-debian
    | MapDep String Relations
    -- ^ Tell cabal-debian to map the first argument (a name that
    -- appears in Extra-Libraries field of the cabal file) to the
    -- second argument (a debian binary package name) using the
    -- --map-dep flag of cabal-debian.
    | SkipVersion String
    -- ^ Do not build target until its debian version number is different
    -- (presumably newer) than the one given here.
    | FailVersion String
    -- ^ Like SkipVersion, but causes a package build failure when matched.
    | SkipPackage
    -- ^ SkipVersion for all versions
    | FailPackage
    -- ^ FailVersion for all versions
    | DebVersion String
    -- ^ The exact debian version number to insert into the changelog.
    -- An exception will be thrown if the version in the retrieved
    -- package looks newer than this.  This causes the --deb-version
    -- flag to be passed to cabal-debian.
    | Revision String
    -- ^ Pass --revision <string> to cabal-debian so a suffix will be
    -- added to the cabal version to get the debian version.  By
    -- default this is -1~hackage1.  Debian policy says this should
    -- either be empty or begin with a dash.
    | Epoch String Int
    -- ^ Set the epoch number in the debian version number generated
    -- for the given cabal package
    | DarcsTag String
    -- ^ When doing a darcs get pass this string to darcs via the --tag flag.
    | KeepRCS
    -- ^ Don't clean out the subdirectory containing the revision control info,
    -- i.e. _darcs or .git or whatever.
    | InGroup GroupName
    -- ^ Include this package in the named group
    deriving (Show, Data, Typeable, Eq, Ord)

$(makeLenses ''Package)
$(makeLenses ''TargetState)

-- | Add a package to a package group
inGroup :: Monad m => GroupName -> PackageId -> TSt m PackageId
-- inGroup p g = modifyPackage (\p' -> over groups (Map.insertWith Set.union g (singleton p')) p') p
inGroup g i = modifyPackage (over groups (Set.insert g)) i

-- | Add a list of packages to a package group
inGroups :: Monad m => [GroupName] -> PackageId -> TSt m PackageId
inGroups gs i = mapM_ (`inGroup` i) gs >> return i

instance Eq Package where
    a == b = compare a b == EQ

instance Ord Package where
    compare = compare `on` (\x -> (view spec x, view flags x))

targetState :: ReleaseTree -> FilePath -> TargetState
targetState rel path = TargetState { _nextPackageId = PackageId 1
                                   , _packageMap = mempty
                                   -- , _groups = mempty
                                   , _release = rel
                                   , _home = path
                                   , _nodes = mempty
                                   , _next = 1
                                   , _deps = mkGraph [] [] }

newId :: Monad m => TSt m PackageId
newId = use nextPackageId >>= \r -> nextPackageId %= succ >> return r

-- | Expand dependency list(?) and turn a Package into a Packages.
-- aPackage :: Monad m => Package -> TSt m Packages
-- aPackage p = return $ APackage p

-- | Expand a package list using the suspected dependency graph
plist :: Monad m => [PackageId] -> TSt m [PackageId]
plist ps = mapM reach ps >>= return . Set.toList . Set.unions
    where
      reach :: Monad m => PackageId -> TSt m (Set PackageId)
      reach p = use nodes >>= maybe (return (singleton p)) (reach' p) . Map.lookup p
      reach' p n = do
        g <- use deps
        -- fromJust because we know these nodes have labels
        return $ Set.fromList $ map (fromJust . lab g) $ {-tr p n $-} reachable n g
      -- tr _p _n ns@[_] = ns
      -- tr p _n [] = error ("No self edge to " ++ show p)
      -- tr _p n ns = trace ("edges: " ++ show n ++ " -> " ++ show ns) ns

-- | If necessary, allocate a new Node and associate it with the argument package.
node :: Monad m => NodeLabel -> TSt m (LNode NodeLabel)
node pkg = do
  (,) <$> node' <*> pure pkg
    where
      node' = do
        mn <- Map.lookup <$> pure pkg <*> use nodes
        maybe (do n' <- use next
                  pkg' <- pure pkg
                  next %= succ
                  nodes %= Map.insert pkg' n'
                  deps %= insNode (n', pkg')
                  pure n')
              pure
              mn

-- | Record the fact that PKG has a build dependency on DEP.
edge :: Monad m => NodeLabel -> NodeLabel -> TSt m ()
edge pkg dep = do
  (pkgNode, _) <- node pkg
  (depNode, _) <- node dep
  deps %= insEdge (pkgNode, depNode, ())

-- | Like edge, but also adds self edges.
edge' :: Monad m => NodeLabel -> NodeLabel -> TSt m ()
edge' pkg dep = do
  edge pkg pkg
  edge dep dep
  edge pkg dep

depends :: Monad m => PackageId -> [PackageId] -> TSt m ()
depends p ps = mapM_ (edge' p) ps

instance Monoid GroupName where
    mempty = NoName
    mappend NoName x = x
    mappend x _ = x

relaxInfo :: [PackageFlag] -> [String]
relaxInfo flags' =
    foldr f [] flags'
    where f (RelaxDep s) ss = s : ss
          f _ ss = ss

-- Combinators for the Packages type

method :: Monad m => Int -> RetrieveMethod -> TSt m PackageId
method n m = do
  i <- newId
  let p = Package { _pid = i, _spec = m, _flags = mempty, _post = mempty, _groups = mempty }
  packageMap %= Map.insert i p
  return i

deletePackage :: Monad m => PackageId -> TSt m ()
deletePackage i = packageMap %= Map.delete i

modifyPackage :: Monad m => (Package -> Package) -> PackageId -> TSt m PackageId
modifyPackage f i = do
  (packageMap . at i) %= maybe Nothing (Just . f)
  return i

clonePackage :: Monad m => (Package -> Package) -> PackageId -> TSt m PackageId
clonePackage f i = do
  j <- newId
  p <- use (packageMap . at i) >>= maybe (error $ "No such package: " ++ show i) return
  packageMap %= Map.insert j (set pid j (f p))
  return j

createPackage :: Monad m => RetrieveMethod -> [PackageFlag] -> [CabalInfo -> CabalInfo] -> TSt m PackageId
createPackage s f p = do
  i <- newId
  let r = Package {_spec = s, _flags = f, _post = p, _pid = i, _groups = mempty}
  packageMap %= Map.insert i r
  return i

mergePackages :: Monad m => (Package -> Package -> Package) -> PackageId -> PackageId -> TSt m PackageId
mergePackages f i1 i2 = do
  Just p1 <- use (packageMap . at i1)
  Just p2 <- use (packageMap . at i2)
  let r = f p1 p2
  packageMap %= Map.delete i2
  packageMap %= Map.insert i1 r
  return i1

mergePackages' :: Monad m => (Package -> Package -> Package) -> PackageId -> PackageId -> TSt m PackageId
mergePackages' f i j = do
  Just p <- Map.lookup i <$> use packageMap
  Just q <- Map.lookup j <$> use packageMap
  let r = f p q
  packageMap %= Map.delete j
  packageMap %= Map.insert i r
  return i

-- | Add a flag to p
flag :: Monad m => PackageFlag -> PackageId -> TSt m PackageId
flag f i = modifyPackage (over flags (f :)) i

mflag :: Monad m => Maybe PackageFlag -> PackageId -> TSt m PackageId
mflag Nothing i = return i
mflag (Just f) i = flag f i

apply :: Monad m => (CabalInfo -> CabalInfo) -> PackageId -> TSt m PackageId
apply f i = modifyPackage (over post (f :)) i

patch :: Monad m => ByteString -> PackageId -> TSt m PackageId
patch s i = modifyPackage (over spec (`Patch` s)) i

cd :: Monad m => FilePath -> PackageId -> TSt m PackageId
cd path i = modifyPackage (over spec (Cd path)) i

apt :: Monad m => String -> String -> TSt m PackageId
apt dist name = method 1 (Apt dist name)

bzr :: Monad m => String -> TSt m PackageId
bzr path = method 2 (Bzr path)

darcs :: Monad m => String -> TSt m PackageId
darcs path = method 3 (Darcs path)

datafiles :: Monad m => RetrieveMethod -> RetrieveMethod -> FilePath -> TSt m PackageId
datafiles cabal files dest = method 4 (DataFiles cabal files dest)

datafiles' :: Monad m => PackageId -> PackageId -> FilePath -> TSt m PackageId
datafiles' cabal files dest = do
  Just cabal' <- use (packageMap . at cabal)
  Just files' <- use (packageMap . at files)
  modifyPackage (const (set spec (DataFiles (view spec cabal') (view spec files') dest) cabal')) cabal
  return cabal

debianize :: Monad m => [String] -> PackageId -> TSt m PackageId
debianize args = modifyPackage (\p -> set spec (Debianize'' (view spec p) Nothing) p)

debdir :: Monad m => RetrieveMethod -> PackageId -> TSt m PackageId
debdir debian i = modifyPackage (\p -> set spec (DebDir (view spec p) debian) p) i

dir :: Monad m => FilePath -> TSt m PackageId
dir path = method 5 (Dir path)

git :: Monad m => String -> [GitSpec] -> TSt m PackageId
git path gitspecs = method 6 (Git path gitspecs)

hackage :: Monad m => Maybe String -> String -> TSt m PackageId
hackage Nothing s = method 7 (Hackage s)
hackage (Just v) s = method 7 (Hackage s) >>= flag (CabalPin v)

hg :: Monad m => String -> TSt m PackageId
hg path = method 8 (Hg path)

proc :: Monad m => PackageId -> TSt m PackageId
proc = modifyPackage (over spec Proc)

quilt :: Monad m => RetrieveMethod -> PackageId -> TSt m PackageId
quilt patchdir = modifyPackage (over spec (\x -> Quilt x patchdir))

sourceDeb :: Monad m => PackageId -> TSt m PackageId
sourceDeb i = use (packageMap . at i) >>= \(Just p) -> method 9 (SourceDeb (view spec p))

svn :: Monad m => String -> TSt m PackageId
svn path = method 10 (Svn path)

tla :: Monad m => String -> TSt m PackageId
tla path = method 11 (Tla path)

twice :: Monad m => PackageId -> TSt m PackageId
twice = modifyPackage (over spec Twice)

uri :: Monad m => String -> String -> TSt m PackageId
uri tarball checksum = method 12 (Uri tarball checksum)

-- | The target name returned is only used by the autobuilder command
-- line interface to choose targets.  They look a lot like debian
-- source package names for historical reasons.  (Deprecated, see
-- debianDefaultAtoms in debian-repo and seereasonDefaultAtoms in
-- autobuilder-seereason.)
{-
targetNameFromCabal "QuickCheck" = "haskell-quickcheck2"
targetNameFromCabal "parsec" = "haskell-parsec3"
targetNameFromCabal "gtk2hs-buildtools" = "gtk2hs-buildtools"
targetNameFromCabal "MissingH" = "haskell-missingh"
targetNameFromCabal s = "haskell-" ++ map toLower s
-}

-- | For the Apt target, the real source tree is in a subdirctory.
findSource :: RetrieveMethod -> FilePath -> IO FilePath
findSource (Patch (Apt _dist _name) _) copyDir =
  try (findDebianSourceTrees copyDir) >>=
  return . either (\ (e :: SomeException) -> D.trace (" -> " ++ show e) copyDir)
           (\ (ts :: [(FilePath, DebianSourceTree)]) ->
             case ts of
               [(subdir, _)] -> D.trace (" -> " ++ show (copyDir </> subdir)) (copyDir </> subdir)
               [] -> error "findSource: Internal error"
               _ -> error $ "Multiple debian source trees in " ++ copyDir ++ ": " ++ show (map fst ts))
findSource _ copyDir = return copyDir

isKeepRCS :: PackageFlag -> Bool
isKeepRCS KeepRCS = True
isKeepRCS _ = False

cabalPin :: PackageFlag -> Maybe String
cabalPin (CabalPin v) = Just v
cabalPin _ = Nothing

darcsTag :: PackageFlag -> Maybe String
darcsTag (DarcsTag v) = Just v
darcsTag _ = Nothing

aptPin :: PackageFlag -> Maybe String
aptPin (AptPin v) = Just v
aptPin _ = Nothing
