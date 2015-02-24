{-# LANGUAGE DeriveDataTypeable, RankNTypes, ScopedTypeVariables #-}
-- | The Packages type specifies how to obtain the source code for one
-- or more packages.
module Debian.AutoBuilder.Types.Packages
    ( Packages(..)
    , Package(..)
    , GroupName(..)
    , TSt
    , TargetState(..)
    , foldPackages
    , foldPackages'
    , filterPackages
    , packageCount
    , PackageFlag(..)
    , relaxInfo
    , hackage
    , method
    , darcs
    , apt
    , debianize
    , flag
    , mflag
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

import Control.Applicative (pure, (<$>))
import Control.Exception (SomeException, try)
import Control.Monad.State (State)
import Data.ByteString (ByteString)
import Data.Generics (Data, Typeable)
import Data.Monoid (Monoid(mempty, mappend))
import Data.String (IsString(fromString))
import Debian.Debianize (CabalInfo)
import Debian.Relation (Relations)
import Debian.Releases (Release(..))
import Debian.Repo (DebianSourceTree, findDebianSourceTrees)
import Debian.Repo.Fingerprint (RetrieveMethod(..), GitSpec(..))
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

data Package
    = Package
      { spec :: RetrieveMethod
      -- ^ This value describes the method used to download the
      -- package's source code.
      , flags :: [PackageFlag]
      -- ^ These flags provide additional details about how to obtain
      -- the package.
      } deriving (Show, Data, Typeable) -- We can't derive Eq while PackageFlag contains functions

data TargetState
    = TargetState
      { home :: FilePath
      , release :: Release
      }

type TSt = State TargetState

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
    | ModifyAtoms (CabalInfo -> CabalInfo)
    -- ^ Modify the cabal-debian configuration in a fully general way
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
    deriving (Show, Data, Typeable)

relaxInfo :: [PackageFlag] -> [String]
relaxInfo flags' =
    foldr f [] flags'
    where f (RelaxDep s) ss = s : ss
          f _ ss = ss

-- Combinators for the Packages type

method :: RetrieveMethod -> Package
method m = Package { spec = m, flags = [] }

-- | Add a flag to every package in p
flag :: TSt Package -> PackageFlag -> TSt Package
flag mp f = (\ p -> p {flags = f : flags p}) <$> mp

mflag :: TSt Package -> Maybe PackageFlag -> TSt Package
mflag mp Nothing = mp
mflag mp (Just f) = flag mp f

patch :: TSt Package -> ByteString -> TSt Package
patch mp s = (\ p -> p {spec = Patch (spec p) s}) <$> mp

rename :: TSt Packages -> GroupName -> TSt Packages
rename mps s = (\ ps -> ps {group = s}) <$> mps

mapSpec :: (RetrieveMethod -> RetrieveMethod) -> Package -> Package
mapSpec f p@(Package {spec = x}) = p {spec = f x}

cd :: TSt Package -> FilePath -> TSt Package
cd mp path = mp >>= \ p -> pure $ p {spec = Cd path (spec p)}

apt :: String -> String -> TSt Package
apt dist name = pure $
          Package
               { spec = Apt dist name
               , flags = [] }

bzr :: String -> TSt Package
bzr path = pure $ method (Bzr path)

darcs :: String -> TSt Package
darcs path = pure $
    Package { spec = Darcs path
            , flags = [] }

datafiles :: RetrieveMethod -> RetrieveMethod -> FilePath -> TSt Package
datafiles cabal files dest = pure $ method (DataFiles cabal files dest)

debianize :: TSt Package -> TSt Package
debianize mp = (\ p -> p { spec = Debianize'' (spec p) Nothing }) <$> mp

-- debdir :: String -> RetrieveMethod -> RetrieveMethod -> Packages
-- debdir name method1 method2 = method name (DebDir method1 method1)

debdir :: TSt Package -> RetrieveMethod -> TSt Package
debdir mp debian = (\ p -> p {spec = DebDir (spec p) debian}) <$> mp

dir :: FilePath -> TSt Package
dir path = pure $ method (Dir path)

git :: String -> [GitSpec] -> TSt Package
git path gitspecs = pure $ method (Git path gitspecs)

hackage :: String -> TSt Package
hackage s = pure $
    Package { spec = Hackage s
            , flags = [] }

hg :: String -> Package
hg path = method (Hg path)

proc :: TSt Package -> TSt Package
proc p = mapSpec Proc <$> p

quilt :: RetrieveMethod -> TSt Package -> TSt Package
quilt patchdir mp = (\ p -> p {spec = Quilt (spec p) patchdir}) <$> mp

sourceDeb :: TSt Package -> TSt Package
sourceDeb mp =  (\ p -> method (SourceDeb (spec p))) <$> mp

svn :: String -> TSt Package
svn path = pure $ method (Svn path)

tla :: String -> TSt Package
tla path = pure $ method (Tla path)

twice :: TSt Package -> TSt Package
twice mp = (\ p -> p {spec = Twice (spec p)}) <$> mp

uri :: String -> String -> TSt Package
uri tarball checksum = pure $ method (Uri tarball checksum)

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
