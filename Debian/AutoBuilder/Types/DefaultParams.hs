{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures #-}
{-# LANGUAGE CPP, FlexibleContexts, TemplateHaskell #-}
module Debian.AutoBuilder.Types.DefaultParams
    ( defaultParams
    ) where

import Control.Lens (review, view)
import Control.Monad.Except (MonadError)
import Data.List as List (isSuffixOf, map)
import Data.Maybe
import Data.Set as Set (empty, fromList)
#if MIN_VERSION_Cabal(2,0,0)
import Distribution.Version (Version)
#else
import Data.Version (Version)
#endif
import Debian.Arch (Arch(Binary), ArchCPU(ArchCPU), ArchOS(ArchOS))
import Debian.AutoBuilder.Types.ParamRec (ParamRec(..), Strictness(..), TargetSpec(..))
import Debian.Codename (parseCodename)
import Debian.Relation (BinPkgName(BinPkgName))
import Debian.Releases (ReleaseTree, ReleaseURI, releaseURI)
import Debian.Repo (SourcesChangedAction(SourcesChangedError))
import Debian.Repo.DebError (DebError)
import Debian.Sources (DebSource, parseSourceLine)
import Debian.TH (here)
import Debian.URI
import Debian.VendorURI (VendorURI, vendorURI)
import Debian.Version (parseDebianVersion')
import Prelude hiding (map)
import System.FilePath ((</>))

-- You would put your organization name here.
defaultVendor = "extra"

defaultParams :: ReleaseTree
              -> String
              -> Maybe Version
              -> String
              -> String
              -> [String]
              -> ParamRec
defaultParams myReleaseTree
              myBuildRelease -- e.g. wheezy or precise
              myCompilerVersion
              myUploadURIPrefix
              myBuildURIPrefix
              myDevelopmentReleaseNames =
    ParamRec
    { buildReleaseTree = myReleaseTree
    , vendorTag = defaultVendorTag
    , oldVendorTags = []
    , hvrVersion = myCompilerVersion
    , autobuilderEmail = "SeeReason Autobuilder <partners@seereason.com>"
    , releaseSuffixes = defaultReleaseSuffixes
    , buildRelease = parseCodename myBuildRelease
    , extraRepos = []
    , theVendorURI = defaultUploadURI myBuildRelease myUploadURIPrefix
    , theReleaseURI = defaultReleaseURI myBuildRelease myBuildURIPrefix myUploadURIPrefix
    -- What we plan to build
    , targets = TargetSpec {allTargets = False, groups = Set.empty}
    , patterns = []
    -- If true, upload the packages after a successful build
    , doUpload = False
    -- If true, run newdist on the upload repository after a successful
    -- build and upload, making them available to apt-get install.
    , doNewDist = False
    -- Clear all the entries in the local pool before starting build.  Use
    -- this when there is stuff already in there that you don't want to
    -- upload to the remote repository.  Usually set from the command line
    -- using --flush-pool.
    , flushPool = False
    , useRepoCache = True
    -- Put the names of any source packages you wish to rebuild whether or
    -- not they appear to need it.  If you modified the package source but
    -- did not modify the version number in the changelog this will force
    -- a build.  This can lead to problems if you build the package for
    -- multiple release or multiple architectures - you can end up with
    -- different source for seemingly identical uploaded versions.  Add
    -- elements from the command line using --force <name>.
    , forceBuild = []
    -- Packages we should build and upload even if their source code looks
    -- older than the version already uploaded to the repository.
    , ignoreNewVersions = False
    -- Do not build if the only reason is a new version - assume the
    -- old version is sufficient.
    , buildTrumped = []
    -- If true, try to set up ssh access to the upload host if necessary.
    , doSSHExport = True
    , report = False
    , doHelp = False
    -- If you are not interested in building everything, put one or more
    -- source package names you want to build in this list.  Only these
    -- packages and their build dependencies will be considered for
    -- building.
    , goals = []
    , dryRun = False
    , allowBuildDependencyRegressions = False
    , setEnv = []
    , showSources = False
    , showParams = False
    , flushAll = False
    , flushSource = False
    , flushRoot = False
    -- Make the output more or less chatty.  Zero is normal, -1 is
    -- quieter, and so on.
    , verbosity = 0
    , topDirParam = Nothing
    , createRelease = []
    , doNotChangeVersion = False
    -- Things that rarely change
    , sources = defaultSources myBuildRelease myUploadURIPrefix myBuildURIPrefix defaultDebianMirrorHost defaultUbuntuMirrorHost
    , globalRelaxInfo = defaultGlobalRelaxInfo
    , strictness = Lax
    , flushDepends = False
    , includePackages = defaultIncludePackages
    , optionalIncludePackages = []
    , excludePackages = []
    , components = defaultComponents myBuildRelease
    , developmentReleaseNames = myDevelopmentReleaseNames
    , releaseAliases = defaultReleaseAliases
    , archSet = fromList [Binary (ArchOS "linux") (ArchCPU "i386"), Binary (ArchOS "linux") (ArchCPU "amd64")]
    , newDistProgram = "newdist"
    -- 6.14 adds the ExtraDevDep parameter.
    -- 6.15 changes Epoch parameter arity to 2
    -- 6.18 renames type Spec -> RetrieveMethod
    -- 6.35 added the CabalDebian flag
    -- 6.61 adds this defaultParams function
    , requiredVersion = [(parseDebianVersion' ("6.60" :: String), Nothing)]
    , hackageServer = "hackage.haskell.org"
    -- Things that are probably obsolete
    , debug = False
    , discard = empty
    , testWithPrivate = False
    , extraReleaseTag = Nothing
    , preferred = []
    , buildDepends = []
    , noClean = False
    , cleanUp = False
    , ifSourcesChanged = SourcesChangedError
    , knownPackages = mempty
    }

defaultVendorTag = "+" ++ defaultVendor
defaultReleaseSuffixes = ["-" ++ defaultVendor, "-private"]
--defaultBaseRelease myBuildRelease = baseReleaseName myBuildRelease
defaultDebianMirrorHost = "ftp.debian.org"
defaultUbuntuMirrorHost = "us.archive.ubuntu.com/ubuntu"

-- This URI is the address of the remote repository to which packages
-- will be uploaded after a run with no failures, when the myDoUpload
-- flag is true.  Packages are uploaded to the directory created by
-- appending '/incoming' to this URI.  This is distinct from the
-- local repository, where each packages is uploaded immediately after
-- it is built for use as build dependencies of other packages during
-- the same run.
--
defaultUploadURI :: MonadError DebError m => String -> String -> m VendorURI
defaultUploadURI myBuildRelease myUploadURIPrefix =
    review vendorURI <$> parseURI' (if isPrivateRelease myBuildRelease then myPrivateUploadURI else myPublicUploadURI)
    where
      myPrivateUploadURI = myUploadURIPrefix </> "deb-private" </> releaseRepoName myBuildRelease
      myPublicUploadURI = myUploadURIPrefix </> "deb" </> releaseRepoName myBuildRelease

-- An alternate url for the same repository the upload-uri points to,
-- used for downloading packages that have already been installed
-- there.
--
defaultReleaseURI :: String -> String -> String -> Either DebError ReleaseURI
defaultReleaseURI myBuildRelease myBuildURIPrefix myUploadURIPrefix =
    review releaseURI <$> parseURI' uriString
    where
      uriString = (if isPrivateRelease myBuildRelease then myUploadBuildURI else myPublicBuildURI)
      myUploadBuildURI = myUploadURIPrefix </> "deb-private" </> releaseRepoName myBuildRelease </> "dists" </> myBuildRelease
      myPublicBuildURI = myBuildURIPrefix </> releaseRepoName myBuildRelease </> "dists" </> myBuildRelease

defaultVendorURI :: String -> String -> String -> Either URIError VendorURI
defaultVendorURI myBuildRelease myBuildURIPrefix myUploadURIPrefix =
    review vendorURI <$> parseURI' uriString
    where
      uriString = (if isPrivateRelease myBuildRelease then myUploadBuildURI else myPublicBuildURI)
      myUploadBuildURI = myUploadURIPrefix </> "deb-private" </> releaseRepoName myBuildRelease
      myPublicBuildURI = myBuildURIPrefix </> releaseRepoName myBuildRelease

-- Build a map assigning names to text for every sources.list we might
-- use.  These names can be used in Apt targets.  It is also assumed
-- that we can use any base release or build release name to look up a
-- sources.list.
defaultSources :: String -> String -> String -> String -> String -> [(String, [DebSource])]
defaultSources myBuildRelease myUploadURIPrefix myPublicURIPrefix debianMirrorHost ubuntuMirrorHost =
    List.map releaseSources
            (debianReleases ++ ubuntuReleases ++
             concatMap (derivedReleaseNames myBuildRelease) (debianReleases ++ ubuntuReleases))
    where
      releaseSources release =
          (release, releaseSourceLines venuri release debianMirrorHost ubuntuMirrorHost)
      Right venuri = defaultVendorURI myBuildRelease myPublicURIPrefix myUploadURIPrefix

-- Build a sources.list for one of our build relases.
--
releaseSourceLines :: VendorURI -> String -> String -> String -> [DebSource]
releaseSourceLines myVendorURI release debianMirrorHost ubuntuMirrorHost =
    case releaseSuffix release of
      Nothing -> baseReleaseSourceLines release debianMirrorHost ubuntuMirrorHost
      Just suff ->
          releaseSourceLines myVendorURI (dropSuffix suff release) debianMirrorHost ubuntuMirrorHost ++
          List.map (parseSourceLine [$here])
            [ "deb " ++ show (view vendorURI myVendorURI) ++ " " ++ release ++ " main"
            , "deb-src " ++ show (view vendorURI myVendorURI) ++ " " ++ release ++ " main" ]

baseReleaseSourceLines release debianMirrorHost ubuntuMirrorHost =
    case releaseRepoName release of
      "debian" -> debianSourceLines debianMirrorHost release
      "ubuntu" -> ubuntuSourceLines ubuntuMirrorHost release
      x -> error $ "Unknown release repository: " ++ show x

debianSourceLines debianMirrorHost release =
    List.map (parseSourceLine [$here]) $
    [ "deb http://" ++ debianMirrorHost ++ "/debian " ++ release ++ " main contrib non-free"
    , "deb-src http://" ++ debianMirrorHost ++ "/debian " ++ release ++ " main contrib non-free" ]

ubuntuSourceLines ubuntuMirrorHost release =
    List.map (parseSourceLine [$here]) $
    [ "deb http://" ++ ubuntuMirrorHost ++ "/ubuntu/ " ++ release ++ " main restricted universe multiverse"
    , "deb-src http://" ++ ubuntuMirrorHost ++ "/ubuntu/ " ++ release ++ " main restricted universe multiverse"
    , "deb http://" ++ ubuntuMirrorHost ++ "/ubuntu/ " ++ release ++ "-updates main restricted universe multiverse"
    , "deb-src http://" ++ ubuntuMirrorHost ++ "/ubuntu/ " ++ release ++ "-updates main restricted universe multiverse"
    , "deb http://" ++ ubuntuMirrorHost ++ "/ubuntu/ " ++ release ++ "-backports main restricted universe multiverse"
    , "deb-src http://" ++ ubuntuMirrorHost ++ "/ubuntu/ " ++ release ++ "-backports main restricted universe multiverse"
    , "deb http://" ++ ubuntuMirrorHost ++ "/ubuntu/ " ++ release ++ "-security main restricted universe multiverse"
    , "deb-src http://" ++ ubuntuMirrorHost ++ "/ubuntu/ " ++ release ++ "-security main restricted universe multiverse" ]

-- Base the private release on the corresponding public release, not
-- directly on upstream releases.
derivedReleaseNames myBuildRelease baseRelease =
    [baseRelease ++ "-" ++ defaultVendor] ++
    if isPrivateRelease myBuildRelease then [baseRelease ++ "-" ++ defaultVendor ++ "-private"] else []

isPrivateRelease release = isSuffixOf "-private" release

defaultGlobalRelaxInfo :: [String]
defaultGlobalRelaxInfo =
    ["base-files",
     "bash",
     "bsdutils",
     "cdbs",
     "devscripts",
     "dpkg",
     "dpkg-dev",
     "gcc",
     "g++",
     "make",
     "mount",
     "base-passwd",
     "mktemp",
     "sed",
     "util-linux",
     "sysvinit-utils",
     "autoconf",
     "debhelper",
     "debianutils",
     "diff",
     "e2fsprogs",
     "findutils",
     "flex",
     "login",
     "coreutils",
     "grep",
     "gs",
     "gzip",
     "hostname",
     "intltool",
     "ncurses-base",
     "ncurses-bin",
     "perl",
     "perl-base",
     "python-minimal",
     "tar",
     "sysvinit",
     "libc6-dev",
     "haskell-devscripts"]

defaultIncludePackages :: [BinPkgName]
defaultIncludePackages =
    map BinPkgName
    [ "debian-archive-keyring"
    , "build-essential"         -- This is required by autobuilder code that opens the essential-packages list
    , "pkg-config"              -- Some packages now depend on this package via new cabal options.
    , "debian-keyring"
    , "locales" ]

defaultReleaseAliases :: [(String, String)]
defaultReleaseAliases =
    [("etch", "bpo40+"),
     ("lenny", "bpo50+"),
     ("squeeze", "bpo60+")]

defaultComponents myBuildRelease =
    case releaseRepoName myBuildRelease of
      "debian" -> ["main", "contrib", "non-free"]
      "ubuntu" -> ["main", "restricted", "universe", "multiverse"]
      _ -> error $ "Invalid build release: " ++ myBuildRelease

releaseSuffix :: String -> Maybe String
releaseSuffix rname =
    case parseReleaseName rname of
      [] -> error $ "Invalid release name: " ++ show rname
      xs -> Just (last xs)

parseReleaseName :: String -> [String]
parseReleaseName rname =
    case mapMaybe (`splitSuffix` rname) defaultReleaseSuffixes of
      [(prefix, suffix)] -> parseReleaseName prefix ++ [suffix]
      [] -> [rname]
      pairs -> error $ "Redundant suffixes in myReleaseSuffixes: " ++ show pairs

releaseRepoName :: String -> String
releaseRepoName rname
    | elem rname debianReleases = "debian"
    | elem rname ubuntuReleases = "ubuntu"
releaseRepoName rname = error $ "Unknown unknown release name: " ++ show rname

debianReleases :: [String]
debianReleases = ["experimental", "sid", "wheezy", "squeeze", "lenny", "sarge"]
ubuntuReleases :: [String]
ubuntuReleases = ["artful", "zesty", "yakkity", "xenial", "vivid", "utopic", "trusty", "supreme",
                  "raring", "quantal", "precise", "oneiric", "natty", "maverick", "lucid",
                  "karmic", "jaunty", "intrepid", "hardy", "feisty", "edgy", "dapper"]

splitSuffix :: String -> String -> Maybe (String, String)
splitSuffix suff string
    | isSuffixOf suff string =
        Just (splitAt (length string - length suff) string)
    | True = Nothing

dropSuffix suff x = take (length x - length suff) x
