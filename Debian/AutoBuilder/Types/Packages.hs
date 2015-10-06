{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts, RankNTypes, ScopedTypeVariables, TemplateHaskell #-}
-- | The Packages type specifies how to obtain the source code for one
-- or more packages.
module Debian.AutoBuilder.Types.Packages
    ( Packages(..)
    , aPackage
    , Package(..), post, spec, flags
    , GroupName(..)
    , TSt
    , TargetState, release, deps, nodes, packageMap, groups
    , inGroup, inGroups
    , targetState
    , depends
    , foldPackages
    , foldPackages'
    , filterPackages
    , packageCount
    , mapPackages
    , PackageFlag(..)
    , plist
    , relaxInfo
    , hackage
    , method
    , darcs
    , apt
    , debianize
    , flag
    , mflag
    , apply
    , patch
    , rename
    , bzr
    , datafiles
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
    , gitBranch
    , darcsTag
    , aptPin
    ) where

import Debug.Trace as D

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative (pure, (<$>))
#endif
import Control.Exception (SomeException, try)
import Control.Lens (makeLenses, over, use, view, (%=))
import Control.Monad (foldM)
import Control.Monad.State (State)
import Data.ByteString (ByteString)
import Data.Function (on)
import Data.Generics (Data, Typeable)
import Data.Graph.Inductive (Node, LNode, insEdge, insNode, lab, mkGraph, reachable)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Map as Map (insert, insertWith, lookup, Map)
import Data.Maybe (fromJust)
#if !MIN_VERSION_base(4,8,0)
import Data.Monoid (Monoid(mempty, mappend))
#endif
import Data.Set as Set (fromList, Set, singleton, toList, union, unions)
import Data.String (IsString(fromString))
import Debian.Debianize (CabalInfo)
import Debian.Relation as Debian (Relations, SrcPkgName)
import Debian.Releases (Release(..))
import Debian.Repo (DebianSourceTree, findDebianSourceTrees)
import Debian.Repo.Fingerprint (RetrieveMethod(..), GitSpec(..))
import Distribution.Package as Cabal (PackageName)
import System.FilePath ((</>))

-- | A type for the group name of a Packages record, used to reference
-- a group of packages.
data GroupName
    = GroupName {unGroupName :: String}
    | NoName
    deriving (Eq, Ord, Show, Data, Typeable)

instance IsString GroupName where
    fromString = GroupName

data Packages
    = NoPackage
    | APackage Package
    | Packages
      { list :: [Packages]
      }
    | Named
      { group :: GroupName
      , packages :: Packages
      } deriving (Show, Data, Typeable)
    -- deriving (Show, Eq, Ord)

newtype PackageId = PackageId Int deriving (Eq, Ord, Read, Show, Data, Typeable)

instance Enum PackageId where
    toEnum = PackageId
    fromEnum (PackageId n) = n
    succ (PackageId n) = PackageId (succ n)

-- | Collected information about a package.
data Package
    = Package
      { _id :: PackageId
      , _spec :: RetrieveMethod
      -- ^ This value describes the method used to download the
      -- package's source code.
      , _flags :: [PackageFlag]
      -- ^ These flags provide additional details about how to obtain
      -- the package.
      , _post :: [CabalInfo -> CabalInfo]
      -- ^ Final transformations to perform on the package info.
      } deriving (Show, Data, Typeable) -- We can't derive Eq because post contains functions

type NodeLabel = Package
type EdgeLabel = ()

data TargetState
    = TargetState
      { _home :: FilePath    -- ^ typically $HOME, where we find .autobuilder/
      , _release :: Release  -- ^ e.g. trusty, trusty-seereason
      , _nodes :: Map NodeLabel Node
      , _next :: Node
      , _deps :: Gr NodeLabel EdgeLabel
      , _nextPackageId :: PackageId
      , _groups :: Map GroupName (Set Package)
      , _packageMap :: Map PackageId Package
      }

type TSt = State TargetState

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
    | GitBranch String
    -- ^ When doing a 'git clone' pass this string to darcs via the --branch flag.
    | KeepRCS
    -- ^ Don't clean out the subdirectory containing the revision control info,
    -- i.e. _darcs or .git or whatever.
    | InGroup GroupName
    -- ^ Include this package in the named group
    deriving (Show, Data, Typeable, Eq, Ord)

$(makeLenses ''Package)
$(makeLenses ''TargetState)

-- | Add a package to a package group
inGroup :: TSt Package -> GroupName -> TSt Package
inGroup p g = p >>= \p' -> groups %= Map.insertWith Set.union g (singleton p') >> p

-- | Add a list of packages to a package group
inGroups :: TSt Package -> [GroupName] -> TSt Package
inGroups p gs = mapM_ (inGroup p) gs >> p

instance Eq Package where
    a == b = compare a b == EQ

instance Ord Package where
    compare = compare `on` (\x -> (view spec x, view flags x))

targetState :: Release -> FilePath -> TargetState
targetState rel path = TargetState { _nextPackageId = PackageId 1
                                   , _packageMap = mempty
                                   , _groups = mempty
                                   , _release = rel
                                   , _home = path
                                   , _nodes = mempty
                                   , _next = 1
                                   , _deps = mkGraph [] [] }

newId :: TSt PackageId
newId = use nextPackageId >>= \r -> nextPackageId %= succ >> return r

-- | Expand dependency list and turn a Package into a Packages.
aPackage :: TSt Package -> TSt Packages
aPackage p = p >>= plist . (: []) >>= \ps -> return $ case ps of
                                                        [p'] -> APackage p'
                                                        _ -> Packages $ map APackage ps

-- | Expand a package list using the suspected dependency graph
plist :: [Package] -> TSt [Package]
plist ps = mapM reach ps >>= return . Set.toList . Set.unions
    where
      reach :: Package -> TSt (Set Package)
      reach p = use nodes >>= maybe (return (singleton p)) (reach' p) . Map.lookup p
      reach' p n = do
        g <- use deps
        -- fromJust because we know these nodes have labels
        return $ Set.fromList $ map (fromJust . lab g) $ {-tr p n $-} reachable n g
      -- tr _p _n ns@[_] = ns
      -- tr p _n [] = error ("No self edge to " ++ show p)
      -- tr _p n ns = trace ("edges: " ++ show n ++ " -> " ++ show ns) ns

-- | If necessary, allocate a new Node and associate it with the argument package.
node :: NodeLabel -> TSt (LNode NodeLabel)
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
edge :: NodeLabel -> NodeLabel -> TSt ()
edge pkg dep = do
  (pkgNode, _) <- node pkg
  (depNode, _) <- node dep
  deps %= insEdge (pkgNode, depNode, ())

-- | Like edge, but also adds self edges.
edge' :: NodeLabel -> NodeLabel -> TSt ()
edge' pkg dep = do
  edge pkg pkg
  edge dep dep
  edge pkg dep

-- A bit of a monad-y mess
-- depends :: TSt Package -> [TSt Package] -> TSt Package
-- depends p ps = mapM_ (edge' p) ps >> p

depends :: Package -> [Package] -> TSt ()
depends p ps = mapM_ (edge' p) ps

-- instance Eq Packages where
--     (APackage p1) == (APackage p2) = p1 == p2
--     (Packages {list = l1}) == (Packages {list = l2}) = l1 == l2
--     (Named {group = g1, packages = p1}) == (Named {group = g2, packages = p2}) = g1 == g2 && p1 == p2
--     NoPackage == NoPackage = True
--     _ == _ = False
--
-- instance Eq Package where
--     p1 == p2 = spec p1 == spec p2

instance Monoid GroupName where
    mempty = NoName
    mappend NoName x = x
    mappend x _ = x

instance Monoid Packages where
    mempty = NoPackage
    mappend NoPackage y = y
    mappend x NoPackage = x
    mappend x@(APackage {}) y = mappend (Packages [x]) y
    mappend x y@(APackage {}) = mappend x (Packages [y])
    mappend x@(Packages {}) y@(Packages {}) = Packages { list = list x ++ list y }
    mappend x@(Named {}) y@(Named {}) =
        Named { group = mappend (group x) (group y)
              , packages = mappend (packages x) (packages y) }
    mappend x@(Named {}) y = x {packages = mappend (packages x) y}
    mappend x y@(Named {}) = y {packages = mappend x (packages y)}

foldPackages' :: (Package -> r -> r)
              -> (GroupName -> Packages -> r -> r)
              -> ([Packages] -> r -> r)
              -> (r -> r)
              -> Packages -> r -> r
foldPackages' _ n _ _ ps@(Named {}) r = n (group ps) (packages ps) r
foldPackages' _ _ g _ ps@(Packages {}) r = g (list ps) r
foldPackages' f _ _ _ (APackage p) r = f p r
foldPackages' _ _ _ h NoPackage r = h r

-- FIXME: this can be implemented using foldPackages'
foldPackages :: (Package -> r -> r) -> Packages -> r -> r
foldPackages _ NoPackage r = r
foldPackages f (APackage x) r = f x r
foldPackages f x@(Packages {}) r = foldr (foldPackages f) r (list x)
foldPackages f x@(Named {}) r = foldPackages f (packages x) r

filterPackages :: (Packages -> Bool) -> Packages -> Packages
filterPackages f xs =
    foldPackages (\ p xs' ->
                      if f (APackage p)
                      then mappend (APackage p) xs'
                      else xs')
                 xs
                 mempty

packageCount :: Packages -> Int
packageCount ps = foldPackages (\ _ n -> n + 1) ps 0

-- | Apply a function to every Package embedded in a Packages.
mapPackages :: Monad m => (m Package -> m Package) -> m Packages -> m Packages
mapPackages f ps =
    ps >>= \ x ->
        case x of
          NoPackage -> return NoPackage
          (APackage p) -> f (return p) >>= return . APackage
          (Packages {}) -> mapM (mapPackages f) (map return (list x)) >>= \ l' -> return $ x {list = l'}
          (Named {}) -> mapPackages f (return (packages x)) >>= \ ps -> return $ x {packages = ps}

relaxInfo :: [PackageFlag] -> [String]
relaxInfo flags' =
    foldr f [] flags'
    where f (RelaxDep s) ss = s : ss
          f _ ss = ss

-- Combinators for the Packages type

method :: RetrieveMethod -> TSt Package
method m = do
  i <- newId
  let p = Package { _id = i, _spec = m, _flags = [], _post = [] }
  packageMap %= Map.insert i p
  return p

-- | Add a flag to every package in p
flag :: TSt Package -> PackageFlag -> TSt Package
flag mp f = over flags (f :) <$> mp

mflag :: TSt Package -> Maybe PackageFlag -> TSt Package
mflag mp Nothing = mp
mflag mp (Just f) = flag mp f

apply :: TSt Package -> (CabalInfo -> CabalInfo) -> TSt Package
apply mp f = over post (f :) <$> mp

patch :: TSt Package -> ByteString -> TSt Package
patch mp s = over spec (`Patch` s) <$> mp

rename :: TSt Packages -> GroupName -> TSt Packages
rename mps s = (\ ps -> ps {group = s}) <$> mps

mapSpec :: (RetrieveMethod -> RetrieveMethod) -> Package -> Package
mapSpec f p = over spec f p

cd :: TSt Package -> FilePath -> TSt Package
cd mp path = over spec (Cd path) <$> mp

apt :: String -> String -> TSt Package
apt dist name = do
  i <- newId
  let p = Package
               { _id = i
               , _spec = Apt dist name
               , _flags = []
               , _post = [] }
  packageMap %= Map.insert i p
  return p

bzr :: String -> TSt Package
bzr path = method (Bzr path)

darcs :: String -> TSt Package
darcs path = do
  i <- newId
  let p = Package { _id = i
                  , _spec = Darcs path
                  , _flags = []
                  , _post = [] }
  packageMap %= Map.insert i p
  return p

datafiles :: RetrieveMethod -> RetrieveMethod -> FilePath -> TSt Package
datafiles cabal files dest = method (DataFiles cabal files dest)

debianize :: TSt Package -> TSt Package
debianize mp = over spec (\x -> Debianize'' x Nothing) <$> mp

-- debdir :: String -> RetrieveMethod -> RetrieveMethod -> Packages
-- debdir name method1 method2 = method name (DebDir method1 method1)

debdir :: TSt Package -> RetrieveMethod -> TSt Package
debdir mp debian = over spec (\x -> DebDir x debian) <$> mp

dir :: FilePath -> TSt Package
dir path = method (Dir path)

git :: String -> [GitSpec] -> TSt Package
git path gitspecs = method (Git path gitspecs)

hackage :: String -> TSt Package
hackage s = do
  i <- newId
  let p = Package { _id = i
                  , _spec = Hackage s
                  , _flags = []
                  , _post = [] }
  packageMap %= Map.insert i p
  return p

hg :: String -> TSt Package
hg path = method (Hg path)

proc :: TSt Package -> TSt Package
proc p = mapSpec Proc <$> p

quilt :: RetrieveMethod -> TSt Package -> TSt Package
quilt patchdir mp = over spec (\x -> Quilt x patchdir)  <$> mp

sourceDeb :: TSt Package -> TSt Package
sourceDeb mp =  mp >>= method . SourceDeb . view spec

svn :: String -> TSt Package
svn path = method (Svn path)

tla :: String -> TSt Package
tla path = method (Tla path)

twice :: TSt Package -> TSt Package
twice mp = over spec Twice <$> mp

uri :: String -> String -> TSt Package
uri tarball checksum = method (Uri tarball checksum)

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

gitBranch :: PackageFlag -> Maybe String
gitBranch (GitBranch v) = Just v
gitBranch _ = Nothing

darcsTag :: PackageFlag -> Maybe String
darcsTag (DarcsTag v) = Just v
darcsTag _ = Nothing

aptPin :: PackageFlag -> Maybe String
aptPin (AptPin v) = Just v
aptPin _ = Nothing
