{-# LANGUAGE DeriveDataTypeable, RankNTypes, ScopedTypeVariables #-}
-- | The Packages type specifies how to obtain the source code for one
-- or more packages.
module Debian.AutoBuilder.Types.Packages
    ( Packages(..)
    , GroupName(..)
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
    , testPackageFlag
    , keepRCS
    ) where

import Debug.Trace as D

import Control.Exception (SomeException, try)
import Data.ByteString (ByteString)
--import Data.Char (toLower)
import Data.Generics (Data, Typeable)
import Data.List (nub, sort)
import Data.Maybe (catMaybes)
import Data.Monoid (Monoid(mempty, mappend))
import Data.String (IsString(fromString))
import qualified Debian.Debianize as CD
import Debian.Relation (Relations)
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
    | Package
      { spec :: RetrieveMethod
      -- ^ This value describes the method used to download the
      -- package's source code.
      , flags :: [PackageFlag]
      -- ^ These flags provide additional details about how to obtain
      -- the package.
      }
    | Packages
      { list :: [Packages]
      }
    | Named
      { group :: GroupName
      , packages :: Packages
      } deriving (Show, Data, Typeable)
    -- deriving (Show, Eq, Ord)

instance Eq Packages where
    (Package {spec = s1}) == (Package {spec = s2}) = s1 == s2
    (Packages {list = l1}) == (Packages {list = l2}) = l1 == l2
    (Named {group = g1, packages = p1}) == (Named {group = g2, packages = p2}) = g1 == g2 && p1 == p2
    NoPackage == NoPackage = True
    _ == _ = False

instance Monoid GroupName where
    mempty = NoName
    mappend NoName x = x
    mappend x _ = x

instance Monoid Packages where
    mempty = NoPackage
    mappend NoPackage y = y
    mappend x NoPackage = x
    mappend x@(Package {}) y = mappend (Packages [x]) y
    mappend x y@(Package {}) = mappend x (Packages [y])
    mappend x@(Packages {}) y@(Packages {}) = Packages { list = list x ++ list y }
    mappend x@(Named {}) y@(Named {}) =
        Named { group = mappend (group x) (group y)
              , packages = mappend (packages x) (packages y) }
    mappend x@(Named {}) y = x {packages = mappend (packages x) y}
    mappend x y@(Named {}) = y {packages = mappend x (packages y)}

foldPackages' :: (RetrieveMethod -> [PackageFlag] -> r -> r)
              -> (GroupName -> Packages -> r -> r)
              -> ([Packages] -> r -> r)
              -> (r -> r)
              -> Packages -> r -> r
foldPackages' _ n _ _ ps@(Named {}) r = n (group ps) (packages ps) r
foldPackages' _ _ g _ ps@(Packages {}) r = g (list ps) r
foldPackages' f _ _ _ p@(Package {}) r = f (spec p) (flags p) r
foldPackages' _ _ _ h NoPackage r = h r

-- FIXME: this can be implemented using foldPackages'
foldPackages :: (RetrieveMethod -> [PackageFlag] -> r -> r) -> Packages -> r -> r
foldPackages _ NoPackage r = r
foldPackages f x@(Package {}) r = f (spec x) (flags x) r
foldPackages f x@(Packages {}) r = foldr (foldPackages f) r (list x)
foldPackages f x@(Named {}) r = foldPackages f (packages x) r

filterPackages :: (Packages -> Bool) -> Packages -> Packages
filterPackages p xs =
    foldPackages (\ spec' flags' xs' ->
                      if p (Package spec' flags')
                      then mappend (Package spec' flags') xs'
                      else xs')
                 xs
                 mempty

packageCount :: Packages -> Int
packageCount ps = foldPackages (\ _ _ n -> n + 1) ps 0

-- | Flags that are applicable to any debianized package, which means
-- any package because this autobuilder only builds debs.
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
    | ModifyAtoms (CD.Atoms -> CD.Atoms)
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

method :: RetrieveMethod -> Packages
method m =
    Package { spec = m
            , flags = [] }

-- | Add a flag to every package in p
flag :: Packages -> PackageFlag -> Packages
flag p@(Named {}) f = p {packages = flag (packages p) f}
flag p@(Package {}) f = p {flags = f : flags p}
flag p@(Packages {}) f = p {list = map (`flag` f) (list p)}
flag NoPackage _ = NoPackage

mflag :: Packages -> Maybe PackageFlag -> Packages
mflag p Nothing = p
mflag p (Just f) = flag p f

patch :: Packages -> ByteString -> Packages
patch package@(Package {}) s = package {spec = Patch (spec package) s}
patch p@(Packages {}) s = p {list = map (`patch` s) (list p)}
patch NoPackage _ = NoPackage
patch p@(Named {}) _ = p

rename :: Packages -> GroupName -> Packages
rename p s = p {group = s}

mapSpec :: (RetrieveMethod -> RetrieveMethod) -> Packages -> Packages
mapSpec f p@(Package {spec = x}) = p {spec = f x}
mapSpec _ NoPackage = NoPackage
mapSpec f p@(Packages {list = xs}) = p {list = map (mapSpec f) xs}
mapSpec f p@(Named {}) = p {packages = mapSpec f (packages p)}

cd :: Packages -> FilePath -> Packages
cd p path = p {spec = Cd path (spec p)}

apt :: String -> String -> Packages
apt dist name =
          Package
               { spec = Apt dist name
               , flags = [] }

bzr :: String -> Packages
bzr path = method (Bzr path)

darcs :: String -> Packages
darcs path =
    Package { spec = Darcs path
            , flags = [] }

datafiles :: RetrieveMethod -> RetrieveMethod -> FilePath -> Packages
datafiles cabal files dest = method (DataFiles cabal files dest)

debianize :: Packages -> Packages
debianize p = p { spec = Debianize' (spec p) [] }

-- debdir :: String -> RetrieveMethod -> RetrieveMethod -> Packages
-- debdir name method1 method2 = method name (DebDir method1 method1)

debdir :: Packages -> RetrieveMethod -> Packages
debdir p debian = p {spec = DebDir (spec p) debian}

dir :: FilePath -> Packages
dir path = method (Dir path)

git :: String -> [GitSpec] -> Packages
git path gitspecs = method (Git path gitspecs)

hackage :: String -> Packages
hackage s =
    Package { spec = Hackage s
            , flags = [] }

hg :: String -> Packages
hg path = method (Hg path)

proc :: Packages -> Packages
proc = mapSpec Proc

quilt :: RetrieveMethod -> Packages -> Packages
quilt patchdir p = p {spec = Quilt (spec p) patchdir}

sourceDeb :: Packages -> Packages
sourceDeb p = method (SourceDeb (spec p))

svn :: String -> Packages
svn path = method (Svn path)

tla :: String -> Packages
tla path = method (Tla path)

twice :: Packages -> Packages
twice p = p {spec = Twice (spec p)}

uri :: String -> String -> Packages
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

testPackageFlag :: (Eq a, Ord a, Show a) => (PackageFlag -> Maybe a) -> Packages -> [a]
testPackageFlag p package@(Package {}) = nub (sort (catMaybes (map p (flags package))))
testPackageFlag _ _ = []

keepRCS :: Packages -> Bool
keepRCS package@(Package {}) =
    case testPackageFlag (\ x -> case x of KeepRCS -> Just (); _ -> Nothing) package of
      [] -> False
      _ -> True
keepRCS _ = False
