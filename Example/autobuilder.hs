#!/usr/bin/env runhaskell -package=base-3.0.3.0
-- Currently this will not run as a script even with the line above.
-- The reason is unclear.  Either use the wrapper script in
-- /usr/bin/autobuilder or run
--   sudo runhaskell -package=base-3.0.3.0 <path to this configuration file> release1 release2 ...

-- Import the symbols we use below.
import Data.List (isSuffixOf)
import Data.Maybe
import qualified Debian.AutoBuilder.Main as M
import qualified Debian.AutoBuilder.ParamClass as P
import Debian.AutoBuilder.ParamClass (Target(..))
import Debian.AutoBuilder.ParamRec
import Debian.Release (Arch(Binary))
import Debian.Repo.Cache (SourcesChangedAction(SourcesChangedError))
import Debian.Repo.Types (ReleaseName(ReleaseName, relName))
import Debian.URI
import Debian.Version (parseDebianVersion)
import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit
import System.IO (hPutStr, hPutStrLn, hFlush, stderr)
import System.Unix.QIO (ePutStr, ePutStrLn)

import Targets
import Usage

-- The known suffixes for build releases.
myReleaseSuffixes = ["-seereason", "-private"]

-- The current list of development releases.  The version numbers for
-- these releases do not need to be tagged with the base release name,
-- only with the vendor tag.  Sid is always a development release,
-- Ubuntu creates a new one for each cycle.
--
myDevelopmentReleaseNames = ["sid", "jaunty"]

-- This tag is used to construct the customized part of the version
-- number for any package the autobuilder builds.
--
myVendorTag = "seereason"

-- Put the names of any source packages you wish to rebuild whether or
-- not they appear to need it.  If you modified the package source but
-- did not modify the version number in the changelog this will force
-- a build.  This can lead to problems if you build the package for
-- multiple release or multiple architectures - you can end up with
-- different source for seemingly identical uploaded versions.
--
myForceBuild = []

-- Clear all the entries in the local pool before starting build.  Use
-- this when there is stuff already in there that you don't want to
-- upload to the remote repository.
--
myFlushPool = False

-- Make the output more or less chatty.  Zero is normal, -1 is
-- quieter, and so on.
--
myVerbosity = 0

-- The list of all known package targets.  The targets we will
-- actually build are chosen from these.  The myBuildRelease argument
-- comes from the autobuilder argument list.
--
myTargets myBuildRelease =
    case isPrivateRelease myBuildRelease of
      False -> myPublicTargets
      True -> myPrivateTargets

-- The list of targets which are uploaded to the public repository
--
myPublicTargets = ghc610CoreTargets ++ autobuilderTargets ++ ghc610Targets ++ otherTargets

-- The list of targets which are uploaded to the private repository
--
myPrivateTargets = privateTargets

-- If you are not interested in building everything, put one or more
-- source package names you want to build in this list.  Only these
-- packages and their build dependencies will be considered for
-- building.
--
myGoals = []

-- Helper function to identify private releases.
--
isPrivateRelease release = isSuffixOf "-private" release

-- These host names are used to construct the sources.list lines to
-- access the Debian and Ubuntu repositories.  The anl.gov values here
-- probably won't work outside the United States.
--
myDebianMirrorHost = "mirror.anl.gov"
myUbuntuMirrorHost = "mirror.anl.gov"
--myDebianMirrorHost = "mirrors.usc.edu"
--myUbuntuMirrorHost = "mirrors.usc.edu"

-- If true, upload the packages after a successful build
--
myDoUpload = False

-- If true, run newdist on the upload repository after a successful
-- build and upload, making them available to apt-get install.
--
myDoNewDist = False

-- If true, try to set up ssh access to the upload host if necessary.
--
myDoSSHExport = True

-- There is a debian standard for constructing the version numbers of
-- packages backported to older releases.  To follow this standard we
-- use bpo40+ for Debian 4.0, aka etch.  Don't start building for a new
-- Debian release without adding a bpo alias, otherwise you won't be able
-- to build when you add one because the existing packages will look too
-- new to trump.
--
myReleaseAliases =
    [("etch", "bpo40+"),
     ("lenny", "bpo50+"),
     ("squeeze", "bpo59+"),
     ("hardy-seereason", "hardy"),
     ("intrepid-seereason", "intrepid"),
     ("jaunty-seereason", "jaunty")]

-- This URI is the address of the remote repository to which packages
-- will be uploaded after a run with no failures, when the myDoUpload
-- flag is true.  Packages are uploaded to the directory created by
-- appending '/incoming' to this URI.  This is distinct from the
-- local repository, where each packages is uploaded immediately after
-- it is built for use as build dependencies of other packages during
-- the same run.
--
myUploadURI myBuildRelease =
    case isPrivateRelease myBuildRelease of
      False -> parseURI $ myPublicUploadURI myBuildRelease
      True -> parseURI $ myPrivateUploadURI myBuildRelease

-- An alternate url for the same repository the upload-uri points to,
-- used for downloading packages that have already been installed
-- there.
--
myBuildURI myBuildRelease =
    case isPrivateRelease myBuildRelease of
      False -> parseURI $ myPublicBuildURI myBuildRelease
      True -> parseURI $ myPrivateBuildURI myBuildRelease

myPrivateUploadURI release = "ssh://upload@deb.seereason.com/srv/deb-private/" ++ releaseRepoName release
myPrivateBuildURI release = "ssh://upload@deb.seereason.com/srv/deb-private/" ++ releaseRepoName release

myPublicUploadURI release = "ssh://upload@deb.seereason.com/srv/deb/" ++ releaseRepoName release
myPublicBuildURI release = "http://deb.seereason.com/" ++ releaseRepoName release

-- Additional packages to include in the clean build environment.
-- Adding packages here can speed things up when you are building many
-- packages, because for each package it reverts the build environment
-- to the clean environment and then installs all the build
-- dependencies.  This only affects newly created environments, so if
-- you change this value use the flushRoot option to get it to take
-- effect.
--
myExtraPackages myBuildRelease =
    ["debian-archive-keyring", "seereason-keyring", "ghc6","ghc6-doc", "ghc6-prof"] ++
    -- Private releases generally have ssh URIs in their sources.list,
    -- I have observed that this solves the "ssh died unexpectedly"
    -- errors.
    (if isPrivateRelease myBuildRelease then ["ssh"] else []) ++
    case releaseRepoName myBuildRelease of
      "debian" -> []
      "ubuntu" -> ["ubuntu-keyring"]
      _ -> error $ "Invalid build release: " ++ myBuildRelease

-- Specify extra packages to include as essential in the build
-- environment.  This option was provided to add either upstart or
-- sysvinit to the build when they ceased to be 'Required' packages.
--
myExtraEssential myBuildRelease =
    ["belocs-locales-bin", "gnupg", "dpkg"] ++
    case releaseRepoName myBuildRelease of
      "debian" -> []
      "ubuntu" -> ["upstart-compat-sysv"]
      _ -> error $ "Unknown build release: " ++ myBuildRelease

------------------------- SOURCES --------------------------------

debianSourceLines debianMirrorHost release =
    [ "deb http://" ++ debianMirrorHost ++ "/debian " ++ release ++ " main contrib non-free"
    , "deb-src http://" ++ debianMirrorHost ++ "/debian " ++ release ++ " main contrib non-free" ]

ubuntuSourceLines ubuntuMirrorHost release =
    [ "deb http://" ++ ubuntuMirrorHost ++ "/ubuntu/ " ++ release ++ " main restricted universe multiverse"
    , "deb-src http://" ++ ubuntuMirrorHost ++ "/ubuntu/ " ++ release ++ " main restricted universe multiverse"
    , "deb http://" ++ ubuntuMirrorHost ++ "/ubuntu/ " ++ release ++ "-updates main restricted universe multiverse"
    , "deb-src http://" ++ ubuntuMirrorHost ++ "/ubuntu/ " ++ release ++ "-updates main restricted universe multiverse"
    , "deb http://" ++ ubuntuMirrorHost ++ "/ubuntu/ " ++ release ++ "-backports main restricted universe multiverse"
    , "deb-src http://" ++ ubuntuMirrorHost ++ "/ubuntu/ " ++ release ++ "-backports main restricted universe multiverse"
    , "deb http://" ++ ubuntuMirrorHost ++ "/ubuntu/ " ++ release ++ "-security main restricted universe multiverse"
    , "deb-src http://" ++ ubuntuMirrorHost ++ "/ubuntu/ " ++ release ++ "-security main restricted universe multiverse" ]

debianReleases = ["sid", "squeeze", "lenny"]
ubuntuReleases = ["jaunty", "intrepid", "hardy"]

dropSuffix suff x = take (length x - length suff) x

allSources debianMirrorHost ubuntuMirrorHost includePrivate =
    map releaseSources
            (debianReleases ++ ubuntuReleases ++
             map (++ "-seereason") (debianReleases ++ ubuntuReleases) ++
             if includePrivate then map (++ "-seereason-private") (debianReleases ++ ubuntuReleases) else []) ++
    [("debian-experimental", unlines (debianSourceLines debianMirrorHost "experimental")),
     ("debian-multimedia",
      (unlines ["deb http://mirror.home-dn.net/debian-multimedia stable main",
                "deb-src http://mirror.home-dn.net/debian-multimedia stable main"])),
      ("kanotix",
       (unlines ["deb http://kanotix.com/files/debian sid main contrib non-free vdr",
                 "  deb-src http://kanotix.com/files/debian sid main contrib non-free vdr"]))]
    where
      releaseSources release = (release, unlines (releaseSourceLines release))
      releaseSourceLines :: String -> [String]
      releaseSourceLines release =
          case () of
            _ | isSuffixOf "-private" release -> privateSourceLines release
              | isSuffixOf "-seereason" release -> seereasonSourceLines release
              | True -> baseReleaseSourceLines release
      baseReleaseSourceLines release =
          case releaseRepoName release of
            "debian" -> debianSourceLines debianMirrorHost release
            "ubuntu" -> ubuntuSourceLines ubuntuMirrorHost release
            x -> error $ "Unknown release repository: " ++ show x
      seereasonSourceLines release =
          releaseSourceLines (dropSuffix "-seereason" release) ++
                                 [ "deb " ++ myPublicBuildURI release ++ " " ++ release ++ " main"
                                 , "deb-src " ++ myPublicBuildURI release ++ " " ++ release ++ " main" ]
      privateSourceLines release =
          releaseSourceLines (dropSuffix "-private" release) ++
                                 [ "deb " ++ myPrivateUploadURI release ++ " " ++ release ++ " main"
                                 , "deb-src " ++ myPrivateUploadURI release ++ " " ++ release ++ " main" ]

-- Any package listed here will not trigger rebuilds when updated.
--
myGlobalRelaxInfo =
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
     "tar",
     "sysvinit",
     "libc6-dev"]

----------------------- BUILD RELEASE ----------------------------

-- Given a release name, return the subdirectory of myUploadURI which
-- contains the repository.
--
releaseRepoName name
    | elem name (debianReleases ++ oldDebianReleases) = "debian"
    | elem name (ubuntuReleases ++ oldUbuntuReleases) = "ubuntu"
    | isSuffixOf "-seereason" name = releaseRepoName (dropSuffix "-seereason" name)
    | isSuffixOf "-private" name = releaseRepoName (dropSuffix "-private" name)
    | True = error $ "Unknown release name: " ++ show name

-- These are releases which are not unsupported for building, but from
-- which we could, if we had to, pull source from using an Apt target.
--
oldDebianReleases = ["etch", "sarge"]
oldUbuntuReleases = ["gutsy", "feisty"]

-- (BinPkgName "module-init-tools",Just (SrcPkgName "linux-2.6"))

-- Nothing below here should need to be modified.

---------------------------- THE PARAMETERS RECORD ---------------------------------
-- Assemble all the configuration info above.

-- |See Documentation in "Debian.AutoBuilder.ParamClass".
params myBuildRelease =
    ParamRec
    { vendorTag = myVendorTag
    , autobuilderEmail = "SeeReason Autobuilder <autobuilder@seereason.org>"
    , releaseSuffixes = myReleaseSuffixes
    , buildRelease = ReleaseName {relName = myBuildRelease}
    , uploadURI = myUploadURI myBuildRelease
    , buildURI = myBuildURI myBuildRelease
    , targets = []
    , doUpload = myDoUpload
    , doNewDist = myDoNewDist
    , flushPool = myFlushPool
    , useRepoCache = True
    , forceBuild = myForceBuild
    , doSSHExport = myDoSSHExport
    , doHelp = False
    -- Things that are occasionally useful
    , goals = myGoals
    , dryRun = False
    , allowBuildDependencyRegressions = False
    , setEnv = []
    , showSources = False
    , showParams = False
    , flushAll = False
    , flushSource = False
    , flushRoot = False
    , verbosity = myVerbosity
    , topDirParam = Nothing
    , createRelease = []
    , doNotChangeVersion = False
    -- Things that rarely change
    , sources = allSources myDebianMirrorHost myUbuntuMirrorHost (isPrivateRelease myBuildRelease)
    , globalRelaxInfo = myGlobalRelaxInfo
    , strictness = P.Moderate
    , extraPackages = myExtraPackages myBuildRelease
    , extraEssential = myExtraEssential myBuildRelease
    , omitEssential = []
    , omitBuildEssential = False
    , developmentReleaseNames = myDevelopmentReleaseNames
    , releaseAliases = myReleaseAliases
    , archList = [Binary "i386",Binary "amd64"]
    , newDistProgram = "newdist -v"
    -- Things that are probably obsolete
    , requiredVersion = [(parseDebianVersion "5.2", Nothing)]
    , debug = False
    , omitTargets = []
    , extraReleaseTag = Nothing
    , preferred = []
    , buildDepends = []
    , noClean = False
    , cleanUp = False
    , ifSourcesChanged = SourcesChangedError
    }

main =
    do args <- getArgs
       case getOpt' Permute optSpecs args of
         (fns, dists, [], []) ->
             do ePutStrLn "Autobuilder starting..."
                -- hPutStrLn stderr ("args=" ++ show args ++ ", dists=" ++ show dists)
                -- Apply the command line arguments to each paramter set
                maybeDoHelp (map (\ p -> foldr ($) p fns) . map params $ dists) >>= M.main
         (_, _, badopts, errs) ->
             ePutStr (usage ("Bad options: " ++ show badopts ++ ", errors: " ++ show errs) optSpecs)

maybeDoHelp xs@(x : _)
    | doHelp x = hPutStr stderr (usage "Usage: " optSpecs) >> exitWith ExitSuccess >> return xs
    | True = return xs
maybeDoHelp [] = return []

optSpecs :: [OptDescr (ParamRec -> ParamRec)]
optSpecs =
    [ Option ['v'] ["verbose"] (NoArg (\ p -> p {verbosity = verbosity p + 1}))
      "Increase progress reporting.  Can be used multiple times."
    , Option ['q'] ["quiet"] (NoArg (\ p -> p {verbosity = verbosity p - 1}))
      "Decrease progress reporting. Can be used multiple times."
    , Option [] ["show-params"] (NoArg (\ p -> p {showParams = True}))
      "Display the parameter set" 
    , Option [] ["flush-pool"] (NoArg (\ p -> p {flushPool = True}))
      "Flush the local repository before building."
    , Option [] ["flush-root"] (NoArg (\ p -> p {flushRoot = True}))
      "Discard and recreate the clean and build environments."
    , Option [] ["flush-source"] (NoArg (\ p -> p {flushSource = True}))
      "Discard and re-download all source code."
    , Option [] ["flush-all"] (NoArg (\ p -> p {flushAll = True}))
      "Remove and re-create the entire autobuilder working directory."
    , Option [] ["use-repo-cache"] (NoArg (\ p -> p {useRepoCache = True}))
      (unlines [ "Use the cached information about the remote repositories, don't try"
               , "to verify them.  Saves time and works as long as Ubuntu doesn't"
               , "create or remove any dists."])
    , Option [] ["do-upload"] (NoArg (\ p -> p {doUpload = True}))
      "Upload the packages to the remote server after a successful build."
    , Option [] ["do-newdist"] (NoArg (\ p -> p {doNewDist = True}))
      "Run newdist on the remote server after a successful build and upload."
    , Option ['n'] ["dry-run"] (NoArg (\ p -> p {dryRun = True}))
      "Exit as soon as we discover a package that needs to be built."
    , Option [] ["all-targets"] (NoArg (\ p -> p {targets = myTargets (relName (buildRelease p))}))
      "Add all known targets to the target list."
    , Option [] ["allow-build-dependency-regressions"]
                 (NoArg (\ p -> p {allowBuildDependencyRegressions = True}))
      (unlines [ "Normally it is an error if a build dependency has an older version"
               , "number than during the previous looks older than it did during the"
               , "previous build.  This option relaxes that assumption, in case the"
               , "newer version of the dependency was withdrawn from the repository,"
               , "or was flushed from the local repository without being uploaded."])
    , Option [] ["target"] (ReqArg (\ s p -> p {targets = targets p ++ [find s p]}) "PACKAGE")
      "Add a target to the target list."
    , Option [] ["goal"] (ReqArg (\ s p -> p {goals = goals p ++ [s]}) "PACKAGE")
      (unlines [ "If one or more goal package names are given the autobuilder"
               , "will only build these packages and any of their build dependencies"
               , "which are in the package list.  If no goals are specified, all the"
               , "targets will be built.  (As of version 5.2 there are known bugs with"
               , "this this option which may cause the autobuilder to exit before the"
               , "goal package is built.)"])
    , Option [] ["force"] (ReqArg (\ s p -> p {forceBuild = forceBuild p ++ [s]}) "PACKAGE")
      ("Build the specified source package even if it doesn't seem to need it.")
    , Option ['h'] ["help", "usage"] (NoArg (\ p -> p {doHelp = True}))
      "Print a help message and exit."
    ]
    where
      find s p = case filter (\ t -> sourcePackageName t == s) (myTargets (relName (buildRelease p))) of
                   [x] -> x
                   [] -> error $ "Package not found: " ++ s
                   xs -> error $ "Multiple packages found: " ++ show (map sourcePackageName xs)
