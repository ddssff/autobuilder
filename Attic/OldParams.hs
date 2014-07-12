{-# LANGUAGE FlexibleInstances #-}
-- |This module examines the command line arguments, the configuration
-- files, and perhaps other aspects of the computing environment
-- (e.g. the output of dpkg-architecture) and computes the run time
-- parameters for the program.
--
-- Author: David Fox <ddssff@gmail.com>
module Debian.AutoBuilder.OldParams
    ( Params(..)
    , optSpecs
    , params
    , usage
    ) where

import		 Control.Monad.Trans
import		 Data.List
import		 Data.Maybe
import		 Extra.TIO hiding (verbosity)
import		 Extra.Misc
import qualified Debian.AutoBuilder.ParamClass as P
import qualified Debian.Config as Config
import		 Debian.Config (ParamSet, Flag(..), ParamDescr(..), computeConfig, optBaseSpecs, values)
import qualified Debian.Config as P (usageInfo)
import qualified Debian.GenBuildDeps as G
import		 Debian.Repo (SliceList(..), SliceName(..), 
                              AptIOT, SourcesChangedAction(..), Arch(..),
                              ReleaseName(..), parseReleaseName)
import		 Debian.URI
import		 Debian.Version
import qualified Data.Map as Map
import qualified System.IO as IO
import		 System.Console.GetOpt
import		 System.Environment as Environment
import		 Text.Regex as Regex

data Params
    = Params { flags :: Map.Map String [String]
             --, allSources :: [NamedSliceList]
             --, buildRepoSources :: SliceList
             }

instance ParamSet Params where
    values params descr =
        concat (map (\ name -> Map.findWithDefault [] name (flags params)) (names descr))

instance P.ParamClass Params where
    topDirParam = topDirParam
    vendorTag = vendorTag
    extraReleaseTag = extraReleaseTag
    extraEssential = extraEssential
    omitEssential = omitEssential
    extraPackages = extraPackages
    strictness = strictness
    debug = debug
    requiredVersion = requiredVersion
    omitBuildEssential = omitBuildEssential
    dryRun = dryRun
    noClean = noClean
    forceBuild = forceBuild
    allowBuildDependencyRegressions = allowBuildDependencyRegressions
    doUpload = doUpload
    doNewDist = doNewDist
    newDistProgram = newDistProgram
    showSources = showSources
    showParams = showParams
    cleanUp = cleanUp
    doSSHExport = doSSHExport
    ifSourcesChanged = ifSourcesChanged
    flushRoot = flushRoot
    flushPool = flushPool
    flushSource = flushSource
    flushAll = flushAll
    useRepoCache = useRepoCache
    preferred = preferred
    createRelease = createRelease
    autobuilderEmail = autobuilderEmail
    uploadURI = uploadURI
    buildURI = buildURI
    buildRelease = buildRelease
    doNotChangeVersion = doNotChangeVersion
    developmentReleaseNames = const ["sid", "jaunty"]
    releaseAliases = releaseAliases
    verbosity = verbosity
    sources = sources
    -- This probably won't work
    targets = map (\ spec -> P.Target {P.sourcePackageName = "", P.sourceSpec = spec, P.relaxInfo = []}) . targets
    goals = goals
    omitTargets = omitTargets
    archList = archList
    buildDepends = buildDepends
    setEnv = setEnv
    globalRelaxInfo = undefined
    releaseSuffixes = releaseSuffixes

-- |Compute and return all the run time parameters by expanding the
-- list of flags, generally computed from the command line arguments,
-- using the Name\/Use macro expansion mechanism.  The result is a Map
-- from a parameter names to a list of the values that were found for
-- that parameter.
--
-- The order of the list of values for a given parameter is well
-- defined.  Values from an expansion of a Use parameter will appear
-- later in the list than values given at a higher level.  Thus,
-- command line parameters will always appear first, and the values
-- given in the common section of the example configuration will
-- always appear last.  IF two values for a parameter are given in the
-- same paragraph of the configuration file, their order is preserved.
--
-- The appName string is used to construct the usage message and
-- candidates for the configuration directory path.
params :: (CIO m) =>
          String
       -> [String]
       -> (String -> IO ())
       -> (IO ())
       -> AptIOT m [(Params, P.Cache)]
params appName useNames doHelp doVersion =
    do args <- liftIO getArgs
       let verbosity = length (filter (== "-v") args) - length (filter (== "-q") args)
       let flags = map (Config.Value "Use") useNames ++ Config.seedFlags appName optSpecs args
       -- The --help and --version are done early, there is a lot of
       -- computation and I/O involved in creating the Params records
       -- that we don't want to do if those options are set.
       case () of
         _ | isJust (Config.findValue flags "Help") -> liftIO (doHelp appName) >> return []
           | isJust (Config.findValue flags "Version") -> liftIO doVersion >> return []
           | True -> params' verbosity appName flags

params' :: CIO m => Int -> String -> [Flag] -> AptIOT m [(Params, P.Cache)]
params' verbosity appName flags =
    do flagLists <- liftIO (computeConfig verbosity appName flags id)
       let params = map (\ x -> Params {flags = listMap (pairsFromFlags x)}) flagLists
       -- Make sure the topdir for each set of parameters exists and
       -- is writable.  If not, we won't be able to update any environments
       -- and none of the information we get will be accurate.
       caches <- mapM P.buildCache params
       let pairs = zip params caches
       case pairs of
         [] -> error "No parameter sets"
         (_ : _) ->
             if any (== []) (map (slices . P.buildRepoSources) pairs)
             then error "Parameter set has no build repo sources"
             else return pairs
    where
      pairsFromFlags (Value k a : etc) = (k, a) : pairsFromFlags etc
      pairsFromFlags (_ : etc) = pairsFromFlags etc
      pairsFromFlags [] = []

buildURI :: Params -> Maybe URI
buildURI params =
    case nub (values params buildURIOpt) of
      [] -> Nothing
      [x] -> maybe (error ("Invalid Build-URI: " ++ x)) Just (parseURI x)
      xs -> error $ "Conflicting values for Build-URI: " ++ show xs
buildURIOpt :: ParamDescr
buildURIOpt = Param [] ["build-uri"] ["Build-URI"] (ReqArg (Value "Build-URI") "[URI]")
              (text ["An alternate url for the same repository the upload-uri points to, used for",
                     "downloading packages that have already been installed there."])

commentOpt = Param [] [] ["Comment"] (ReqArg (Value "Comment") "COMMENT")
	     "No effect.  Used to document the configuration file."

versionOpt = Param [] ["version"] ["Version"] (NoArg (Value "Version" "yes")) "Print the package version number and exit."

-- |Command line option specifications
optSpecs :: [ParamDescr]
optSpecs = globalOpts ++ sourceOpts ++ buildOpts ++ localRepoOpts ++ uploadOpts

globalOpts :: [ParamDescr]
globalOpts =
    [helpOpt,
     debugOpt,
     versionOpt,
     releaseAliasOpt,
     requiredVersionOpt,
     topDirOpt,
     dryRunOpt,
     commentOpt,
     verbosityOpt,
     quieterOpt,
     flushAllOpt,
     useRepoCacheOpt,
     showParamsOpt,
     showSourcesOpt,
     --styleOpt,
     offlineOpt]

-- |Obtaining and preparing target source
sourceOpts :: [ParamDescr]
sourceOpts =
    [sourcesOpt,
     flushSourceOpt, 
     vendorTagOpt,
     extraReleaseTagOpt,
     targetsOpt,
     goalOpt,
     omitTargetsOpt]

-- |Build Environment
buildOpts :: [ParamDescr]
buildOpts =
    [forceBuildOpt,
     allowBuildDependencyRegressionsOpt,
     laxOpt, moderateOpt, strictOpt,
     preferOpt,
     buildDependsOpt,
     setEnvOpt,
     noCleanOpt,
     baseReleaseOpt,
     buildReleaseOpt,
     doNotChangeVersionOpt,
     isDevelopmentReleaseOpt,
     ifSourcesChangedOpt,
     relaxDependsOpt,
     omitBuildEssentialOpt,
     extraEssentialOpt,
     omitEssentialOpt,
     extraPackagesOpt,
     flushRootOpt]

-- |Local repository
localRepoOpts :: [ParamDescr]
localRepoOpts =
    [flushPoolOpt,
     cleanUpOpt,
     archListOpt]

-- |Uploading
uploadOpts :: [ParamDescr]
uploadOpts =
    [uploadURIOpt,
     buildURIOpt,
     doUploadOpt,
     doNewDistOpt,
     newDistProgramOpt,
     createReleaseOpt,
     doSSHExportOpt,
     autobuilderEmailOpt]

-- |Usage message
usage :: String -> String
usage appName
    = P.usageInfo ("\nUsage: " ++ appName ++ " [OPTIONS] [SECTION NAMES]\n\n" ++
                   "See the --config option for a description of the locations\n" ++
                   "of the configuration file or directory.  See the --use option\n" ++
                   "for a description of the treatment of options that occur\n" ++
                   "repeatedly.  Any arguments not associated with a flag are treated\n" ++
                   "as additional --use arguments.\n\n" ++
                   "EXAMPLES:\n" ++
                   "  sudo autobuilder build-feisty-common --target apt:sid:haskell-hsql --flush-pool\n" ++
                   "  sudo autobuilder build-feisty-common --do-upload\n\n" ++
                   "GLOBAL OPTIONS:\n") (optBaseSpecs appName ++ globalOpts) ++
      P.usageInfo "\nOBTAINING AND PREPARING TARGET SOURCE CODE:\n" sourceOpts ++
      P.usageInfo "\nCREATING AND USING THE BUILD ENVIRONMENT:\n" buildOpts ++
      P.usageInfo "\nMANAGING THE LOCAL TEMPORARY PACKAGE REPOSITORY:\n" localRepoOpts ++
      P.usageInfo "\nUPLOADING PACKAGES TO THE REMOTE REPOSITORY:\n" uploadOpts

{-
-- |A function to compactly display a parameter set, cutting off any
-- long strings.
prettyPrint :: Params -> String
prettyPrint params =
    concat (reverse (Map.foldWithKey (\ k xs s -> format k xs ++ s) [] (flags params)))
    where
      format k xs = map (\ x -> ("  " ++ k ++ ": " ++ (abbrev . escape) x ++ "\n")) xs
      escape s =
          case break (== '\n') s of
            (s, []) -> s
            (s, (_ : etc)) -> s ++ "\\n" ++ escape etc
      abbrev s =
          case length s of
            n | n > 50 -> take 50 s ++ "..."
	      | otherwise -> s
-}

-- | The top of the directory tree where the autobuilder will
-- create its information cache.
topDirParam :: Params -> Maybe FilePath
topDirParam params =
    case nub (values params topDirOpt) of
      [] -> Nothing
      [x] -> Just x
      xs -> error $ "Conflicting values for Top-Dir: " ++ show xs

topDirOpt = Param [] ["top-dir"] ["Top-Dir"] (ReqArg (Value "Top-Dir") "PATH")
            (text ["The directory the program will use for its working storage,",
                   "default: " ++ P.topDirDefault])

-- |The string used to construct modified version numbers.  E.g.,
-- Ubuntu uses "ubuntu", this should reflect the name of the repository
-- you are going to upload to.
vendorTag :: Params -> String
vendorTag params =
    case values params vendorTagOpt of
      [] -> error "Missing Vendor-Tag parameter, can't generate version numbers."
      tag : _ -> tag
vendorTagOpt = Param [] ["vendor-tag"] ["Vendor-Tag"] (ReqArg (Value "Vendor-Tag") "TAG")
               "The string used to construct modified version numbers"

-- |If this parameter is set, use the old "r0vendor1" style tag instead
-- of just "vendor1".
extraReleaseTag :: Params -> Maybe Int
extraReleaseTag params = case (values params extraReleaseTagOpt) of
                      [] -> Nothing
                      (n : _) -> read n
extraReleaseTagOpt = Param [] ["extra-release-tag"] ["Extra-Release-Tag"] (ReqArg (Value "Extra-Release-Tag") "NUMBER")
                ("use the old \"r0vendor1\" style tag instead of just \"vendor1\".")

extraEssential :: Params -> [String]
extraEssential params = values params extraEssentialOpt

extraEssentialOpt =
    Param { shortOpts = []
          , longOpts = ["extra-essential"]
          , argDescr = ReqArg (Value "ExtraEssential") "PACKAGE"
          , description =
            (text ["Specify an extra package to include as essential in the build",
                   "environment.  This option was provided to add either upstart or",
                   "sysvinit to the build when they ceased to be 'Required' packages."])
{-	  , option = Option [] ["extra-essential"] (ReqArg (Value "Extra-Essential") "PACKAGE")
                       (text ["Specify an extra package to include as essential in the build",
                              "environment.  This option was provided to add either upstart or",
                              "sysvinit to the build when they ceased to be 'Required' packages."]) -}
          , names = ["Extra-Essential"] }

omitEssential :: Params -> [String]
omitEssential params = values params omitEssentialOpt
omitEssentialOpt = Param [] ["omit-essential"] ["Omit-Essential"] (ReqArg (Value "Omit-Essential") "PACKAGE")
                   (text ["Specify a package to tell build-env to remove from the",
                          "essential list even if it is marked essential"])

extraPackages :: Params -> [String]
extraPackages params = values params extraPackagesOpt
extraPackagesOpt = Param [] ["extra-package"] ["Extra-Package"] (ReqArg (Value "Extra-Package") "PACKAGE")
                   (text ["Additional packages to include in the clean build environment.",
                          "This can speed things up when you are building many packages,",
                          "because for each package it reverts the build environment to",
                          "the clean environment and then installs all the build dependencies.",
                          "This only affects newly created environments, so if you change",
                          "this value use the --flush-root option to get it to take effect."])


-- |Return the value of a strictness flag (--strict, --moderate, --lax)
strictness :: Params -> P.Strictness
strictness params =
    case (values params laxOpt, values params moderateOpt, values params strictOpt) of
      ([], [], []) -> P.Moderate
      (_, [], []) -> P.Lax
      ([], [], _) -> P.Strict
      ([], _, []) -> P.Moderate
      _ -> error "Conflicting strictness options"

laxOpt = Param [] ["lax"] ["Lax"] (NoArg (Value "Strictness" "lax"))
	 (text ["Specify how strict to be about the creation of build environments.",
		"A clean build environment is always maintained, and copied before",
		"the package build is performed using rsync.  'Strict' means the",
		"clean build environment is discarded and recreated before each",
                "target is built.  'Moderate' means the clean build environment is",
                "kept between successive runs, and updated as necessary using",
                "'apt-get update' and 'apt-get dist-upgrade'.  'Lax' means that ",
                "build dependencies are installed into the clean build environment",
                "so that they accumulate across runs."])
moderateOpt = Param [] ["moderate"] ["Moderate"] (NoArg (Value "Strictness" "moderate")) "save build environment between targets"
strictOpt = Param [] ["strict"] ["Strict"] (NoArg (Value "Strictness" "strict")) "recreate build environment for each target"

-- New - accessors for Params

{-
help :: Params -> Bool
help params = values params helpOpt /= []
-}
helpOpt = Param [] ["help"] ["Help"] (NoArg (Value "Help" "yes")) "Print usage message."

debug :: Params -> Bool
debug params = values params debugOpt /= []
debugOpt = Param [] ["debug"] ["Debug"] (NoArg (Value "Debug" "yes"))
           "Unspecified debugging behavior."

-- The --required-version parameter gives the minimum version number of
-- the autobuilder and optionally a message explaining why an upgrade
-- is necessary.
requiredVersion :: Params -> [(DebianVersion, Maybe String)]
requiredVersion params =
    map parseVersionAndReason (values params requiredVersionOpt)
    where
      parseVersionAndReason s =
          case break (\ a -> elem a " \t\n") s of
            (v, []) -> (parseDebianVersion v, Nothing)
            (v, r) -> (parseDebianVersion v, Just r)
requiredVersionOpt = Param [] ["required-version"] ["Required-Version"] (ReqArg (Value "Required-Version") "VERSION [REASON]")
                     "Exit with a message if the version number of the autobuilder is too low."

-- |Unimplemented: this would allow off-line development, though it is
-- not clear exactly how.
{-
offline :: Params -> Bool
offline params = values params offlineOpt /= []
-}
offlineOpt = Param [] ["offline"] ["Offline"] (NoArg (Value "Offline" "yes"))
 	     "Unimplemented: work offline, don't issue commands that use the network (like apt-get)"

-- |Don't save the version numbers of the essential and
-- build-essential packages to the package's revision string, only the
-- build dependencies explicitly mentioned in the package's control
-- file.
omitBuildEssential :: Params -> Bool
omitBuildEssential params = values params omitBuildEssentialOpt /= []
omitBuildEssentialOpt = Param [] ["omit-build-essential"] ["Omit-Build-Essential"] (NoArg (Value "Omit-Build-Essential" "yes"))
                        (text ["Don't automatically consider all the build essential packages to be build",
                               "dependencies.  If you are working with an unstable repository where the",
                               "core packages are undergoing frequent revisions, and you aren't worried",
                               "that a new version of 'tar' is going to change the outcome of your builds,",
                               "this option can reduce the number of pointless rebuilds."])

-- |What this flag should mean in this program is somewhat unclear.
-- It may be removed in the future.
dryRun :: Params -> Bool
dryRun params = values params dryRunOpt /= []
dryRunOpt = Param ['n'] ["dry-run"] ["Dry-Run"] (NoArg (Value "Dry-Run" "yes"))
	    (text ["This flag says not to do anything that will affect the outside",
                   "world, such as uploads and remote newdists.  However, the",
                   "files in ~/.autobuilder may still be modified when this is used.",
                   "It does avoids making extensive changes to the local repository",
                   "by exiting as soon as a target it identified as needing to be",
                   "built."])

-- |Pass the -nc flag to dpkg-buildpackage.  (This won't work until
-- the strictness levels are modified so there is some way to build
-- without first syncing the clean environment into the build.)
noClean :: Params -> Bool
noClean params = values params noCleanOpt /= []
noCleanOpt = Param [] ["no-clean"] ["No-Clean"] (NoArg (Value "No-Clean" "yes"))
	     (text ["Run dpkg-buildpackage with the -nc argument.  This also disables",
                    "syncing with the clean source tree.  This should only be used for",
                    "debugging the autobuilder or for debugging the package build.  To",
                    "edit the package you need to find the work directory in the cached",
                    "build and make your edits there.  Then you will need to check them",
                    "back into your revision control system."])

forceBuild :: Params -> [String]
forceBuild params = values params forceBuildOpt
forceBuildOpt = Param [] ["force-build"] ["Force-Build"] (ReqArg (Value "Force-Build") "SOURCEPACKAGE")
                "Build the named package(s) whether or not it seems to need it."

allowBuildDependencyRegressions :: Params -> Bool
allowBuildDependencyRegressions params = values params allowBuildDependencyRegressionsOpt /= []
allowBuildDependencyRegressionsOpt = Param [] ["allow-build-dependency-regressions"] ["Allow-Build-Dependency-Regressions"]
                                     (NoArg (Value "Allow-Build-Dependency-Regressions" "yes"))
                                     (text ["Normally, if a build dependency has an older version number than it did",
                                            "on a previous build, it is an error.  This generally means the sources.list",
                                            "is incorrect.  However, this flag can be necessary if a package gets withdrawn."])

-- | Upload the packages in the given local repository to the
-- corresponding remost repository.
doUpload :: Params -> Bool
doUpload params = values params doUploadOpt /= []
doUploadOpt = Param [] ["do-upload"] ["Do-Upload"] (NoArg (Value "Do-Upload" "yes"))
              (text ["After building any specified targets, dupload all the packages",
                     "in the local pool specified by the --upload-url argument to the",
                     "corresponding repository."])

doNewDist :: Params -> Bool
doNewDist params = values params doNewDistOpt /= []
doNewDistOpt = Param [] ["do-newdist"] ["Do-NewDist"] (NoArg (Value "Do-NewDist" "yes"))
               "Run newdist on the remote repository incoming directory"

newDistProgram :: Params -> String
newDistProgram params = case nub $ values params newDistProgramOpt of
                          [] -> "newdist -v"
                          [x] -> x
                          xs -> error $ "Multiple conflicting values for NewDist-Program: " ++ show xs
newDistProgramOpt = Param [] ["newdist-program"] ["Newdist-Program"] (ReqArg (Value "NewDist-Program") "PATH")
               "Use given executable as the newdist program (default: 'newdist'.)"

showSources :: Params -> Bool
showSources params = values params showSourcesOpt /= []
showSourcesOpt = Param [] ["show-sources"] ["Show-Sources"] (NoArg (Value "Show-Sources" "yes"))
                 "Print the sources.list for the build distro and exit"

showParams :: Params -> Bool
showParams params = values params showParamsOpt /= []
showParamsOpt = Param [] ["show-params"] ["Show-Params"] (NoArg (Value "Show-Params" "yes"))
                "Print the expanded runtime parameter list and continue."

cleanUp :: Params -> Bool
cleanUp params = values params cleanUpOpt /= []
cleanUpOpt = Param [] ["clean-up"] ["Clean-Up"] (NoArg (Value "Clean-Up" "yes"))
             (text ["Do a garbage collection on the local repository, move",
                    "all unreferenced files to 'removed'."])

doSSHExport :: Params -> Bool
doSSHExport params = values params doSSHExportOpt /= []
doSSHExportOpt = Param [] ["ssh-export"] ["SSH-Export"] (NoArg (Value "SSH-Export" "yes"))
                 "Try to set up ssh keys if upload host asks for a password."
 
ifSourcesChanged :: Params -> SourcesChangedAction
ifSourcesChanged params =
    let xs = values params ifSourcesChangedOpt in
    case xs of
      (first : etc) | not (all (== first) etc) -> error ("Conflicting values for If-Sources-Changed: " ++ show xs)
      [] -> SourcesChangedError
      ("error" : _) -> SourcesChangedError
      ("update" : _) -> UpdateSources
      ("remove" : _) -> RemoveRelease
      (other : _) -> error ("Invalid argument to If-Sources-Changed: " ++ other)
ifSourcesChangedOpt = Param [] ["if-sources-changed"] ["If-Sources-Changed"] (ReqArg (Value "If-Sources-Changed") "ACTION")
                      (text ["What to do if the sources.list changes in the",
                             "configuration directory.  The argument may be",
                             "error - (the default) print a message and exit, ",
                             "update - rewrite sources.list and update the environment, ",
                             "remove - discard and rebuild the environment"])

flushRoot :: Params -> Bool
flushRoot params = values params flushRootOpt /= []
flushRootOpt = Param [] ["flush-root"] ["Flush-Root"] (NoArg (Value "Flush-Root" "yes"))
               "Discard and recreate the clean and build environments"

flushPool :: Params -> Bool
flushPool params = values params flushPoolOpt /= []
flushPoolOpt = Param [] ["flush-pool"] ["Flush-Pool"] (NoArg (Value "Flush-Pool" "yes"))
               (text ["Discard the packages in the local pool before building.  Use this",
                      "when the packages in the local pool have already been uploaded, or",
                      "when a bad package was uploaded to the local pool (but not to the",
                      "remote pool), and you want to build a new version with the same",
                      "version number."])

flushSource :: Params -> Bool
flushSource params = values params flushSourceOpt /= []
flushSourceOpt = Param [] ["flush-source"] ["Flush-Source"] (NoArg (Value "Flush-Source" "yes"))
                 "Discard and re-download all source code."

flushAll :: Params -> Bool
flushAll params = values params flushAllOpt /= []
flushAllOpt = Param [] ["flush-all"] ["Flush-All"] (NoArg (Value "Flush-All" "yes"))
              "Remove and re-create the entire autobuilder working directory."

useRepoCache :: Params -> Bool
useRepoCache params = values params useRepoCacheOpt /= []
useRepoCacheOpt = Param [] ["use-repo-cache"] ["Use-Repo-Cache"] (NoArg (Value "Use-Repo-Cache" "yes"))
              (text ["Load the most recent cached repository information from ~/.autobuilder/repoCache",
                     "and assume that it is still good - that no releases have been added or removed",
                     "from the repositories listed.  This is usually safe and saves some time."])

preferred :: Params -> [String]
preferred params = values params preferOpt
preferOpt = Param [] ["prefer"] ["Prefer"] (ReqArg (Value "Prefer") "PACKAGE")
	    (text ["When selecting build dependencies, prefer this particular package",
                   "over other alternatives that could fulfill the dependency, even if",
                   "this package seems older than some other alternative.  For example,",
                   "the c-compiler virtual package is provided by gcc-3.3, gcc-3.4,",
                   "gcc-4.0, etc.  If 'Prefer: gcc-3.4' is used, a dependency on",
                   "c-compiler will choose gcc-3.4 over the others if possible."])

createRelease :: Params -> [String]
createRelease params = values params createReleaseOpt
createReleaseOpt = Param [] ["create-release"] ["Create-Release"] (ReqArg (Value "Create-Release") "NAME") 
                   "If necessary, pass an argument to newdist to create a new release in the upload repository."

autobuilderEmail :: Params -> String
autobuilderEmail params = case nub $ values params autobuilderEmailOpt of
                            [] -> "Autobuilder Email Not Set <autobuilder@somewhere>"
                            [x] -> x
                            xs -> error $ "Multiple conflicting values for Autobuilder-Email: " ++ show xs
autobuilderEmailOpt = Param [] ["autobuilder-email"] ["Autobuilder-Email"] (ReqArg (Value "Autobuilder-Email") "EMAIL")
                      "Email address of autobuilder for use in generated changelog entries."

-- |Find the name of the base release against which we will be building.
baseRelease :: Params -> SliceName
baseRelease params =
    case (nub $ values params baseReleaseOpt) of
      [] -> error "Missing 'Base-Release' parameter"
      (x : _) -> (SliceName x)
baseReleaseOpt = Param [] ["base-release"] ["Base-Release"] (ReqArg (Value "Base-Release")  "NAME")
                 (text ["The name of the release we are basing our release on.  This sources.list",
                        "is combined with the one constructed from the Build-URI to create",
                        "the build environment."])

uploadURI :: Params -> Maybe URI
uploadURI params =
    case nub (values params uploadURIOpt) of
      [] -> Nothing
      [x] -> maybe (error ("Invalid Upload-URI: " ++ x)) Just (parseURI x)
      xs -> error $ "Conflicting values for Upload-URI: " ++ show xs
uploadURIOpt = Param [] ["upload-uri"] ["Upload-URI"] (ReqArg (Value "Upload-URI") "URI")
               (text ["This URI is the address of the remote repository to which packages will be",
                      "uploaded after a run with no failures, when the --do-upload flag is used.",
                      "Packages are uploaded to the directory created by appending '/incoming'",
                      "to this URI.  This is different from the local repository, where each packages is",
                      "uploaded immediately after it is built for use as build dependnecies of other",
                      "packages during the same run."])

-- |Return the name of the release we will be building packages for.
buildRelease :: Params -> ReleaseName
buildRelease params =
    case values params buildReleaseOpt of
      [] -> error "Missing Build-Release parameter"
      (x : _) -> parseReleaseName x
buildReleaseOpt = Param [] ["build-release"] ["Build-Release"] (ReqArg (Value "Build-Release") "NAME")
                  "The name of the release that the packages we build will be uploaded to."

doNotChangeVersion :: Params -> Bool
doNotChangeVersion params = values params doNotChangeVersionOpt /= []
doNotChangeVersionOpt = Param [] ["do-not-change-version"] ["Do-Not-Change-Version"] (NoArg (Value "Do-Not-Change-Version" "yes"))
		  (text ["Don't modify the package's version in any way before building.",
                         "This can lead to attempts to upload packages that are already",
                         "present in the repository, packages that are trumped by versions",
                         "already uploaded to the release."])

isDevelopmentRelease :: Params -> Bool
isDevelopmentRelease params = values params isDevelopmentReleaseOpt /= []
isDevelopmentReleaseOpt = Param [] ["development-release"] ["Development-Release"] (NoArg (Value "Development-Release" "yes"))
		  (text ["Signifies that the release we are building for is a development",
                         "(or unstable) release.  This means we the tag we add doesn't need",
                         "to include '~release', since there are no newer releases to",
                         "worry about trumping."])

releaseAliases :: Params -> [(String, String)]
releaseAliases params =
    map (makePair . break (== '=')) (values params releaseAliasOpt)
    where
      makePair (a, ('=' : b)) = (a, b)
      makePair (a, b) = error $ "ReleaseAlias invalid argument: " ++ a ++ b
releaseAliasOpt = Param [] ["release-alias"] ["Release-Alias"] (ReqArg (Value "Release-Alias") "RELEASENAME=ALIAS")
                  (text ["Use an alias for a release name when constructing the vendor tag,",
                         "for example, --release-alias etch=bpo40+"])

-- This is not used, the verbosity is computed by inspecting getArgs directly
-- because it is used during the construction of the Params value.
verbosity :: Params -> Int
verbosity params = foldr (+) 0 (map read (values params verbosityOpt)) - foldr (+) 0 (map read (values params quieterOpt))

verbosityOpt = Param ['v'] [] ["Verbosity"] (OptArg (\ x -> Value "Verbosity" (maybe "1" id x)) "INCREMENT")
	       "How chatty? (see also --style)"
quieterOpt = Param ['q'] [] ["Quieter"] (OptArg (\ x -> Value "Quieter" (maybe "1" id x)) "DECREMENT")
             "Decreases chattyness"

-- | Flag: --sources, config: Sources
sources :: Params -> [(String, String)]
sources params =
    map parse (values params sourcesOpt)
    where
      parse text = 
          case matchRegex re text of
            Just [name, sources] -> (name, sources)
      re = mkRegexWithOpts "^[ \t\n]*([^ \t\n]+)[ \t\n]+(.*)$" False True

sourcesOpt = Param [] ["sources"] ["Sources"] (ReqArg (Value "Sources") "NAME LINES")
             (text ["(Config file only.)  Specify the a distribution name and, on",
                    "succeeding lines, the sources.list for that distribution."])

-- | Flag: --target, config: Targets
targets :: Params -> [String]
targets params = concat (map words (values params targetsOpt))

targetsOpt = Param ['t'] ["target", "targets"] ["Target", "Targets"] (ReqArg (Value "Targets") "TARGET TARGET...")
	     (text ["Specify one or more build targets, methods for obtaining the source",
                    "code of a package to be built.  See TARGET TYPES below for information",
                    "about the available target types." ])

-- | Flag: --goal, config: Goal
goals :: Params -> [String]
goals params = concat (map words (values params goalOpt))

goalOpt = Param ['g'] ["goal"] ["Goal"] (ReqArg (Value "Goal") "NAME")
	  (text ["Specify a source package which we want to build, and stop once all goals",
                 "are built.  If not given all targets are considered goals."])

-- |Return the value of the --omit-target flag.
omitTargets :: Params -> [String]
omitTargets params = concat (map words (values params omitTargetsOpt))

omitTargetsOpt = Param [] ["omit-target", "omit-targets"] ["Omit-Target", "Omit-Targets"]
                 (ReqArg (Value "Omit-Targets") "TARGET TARGET...")
                 (text ["Remove these from the list of targets given with the",
                        "--target directive.  This option is used to specify",
                        "targets which are temporarily broken in a particular",
                        "distribution."])
defaultArchitectureList :: [String]
defaultArchitectureList = ["i386", "amd64"]

archList :: Params -> [Arch]
archList params =
    map Binary (concat (map words (case values params archListOpt of
                                     [] -> defaultArchitectureList
                                     xs -> xs)))
archListOpt = Param [] ["architecture-list"] ["Architecture-List"] (ReqArg (Value "Architecture-List") "'ARCH ARCH...'")
	      (text ["The list of architectures to prepare the repository to accept.",
                     "By default this is " ++ concat (intersperse " " defaultArchitectureList)])

buildDepends :: Params -> [String]
buildDepends params = values params buildDependsOpt
buildDependsOpt = Param [] ["build-depends"] ["Build-Depends"] (ReqArg (Value "Build-Depends") "PACKAGE")
                  "Add a missing build dependency"

setEnv :: Params -> [String]
setEnv params = values params setEnvOpt
setEnvOpt = Param [] ["setenv"] ["Set-Env"] (ReqArg (Value "Set-Env") "VAR=VALUE")
            "Set an environment variable during the build"

relaxDepends :: Params -> G.RelaxInfo
relaxDepends params =
    G.RelaxInfo $ map (makePair . words) (values params relaxDependsOpt)
    where
      makePair [a] = (G.BinPkgName a, Nothing)
      makePair [a, b] = (G.BinPkgName a, Just (G.SrcPkgName b))
      makePair xs = error ("Invalid Relax-Depends value: " ++ show xs)
relaxDependsOpt = Param [] ["relax-depends"] ["Relax-Depends"] (ReqArg (Value "Relax-Depends") "DEPENDENCY [SOURCE]")
                  (text ["Do not trigger builds due to new versions of this package",
                         "appears, optionally specifying which source package not to build.",
                         "This is used to break dependency loops, For example,",
                         "'Relax-Depends: ghc6 hscolour' means 'even if ghc6 is rebuilt, don't",
                         "rebuild hscolour even though ghc6 is one of its build dependencies'."])

text :: [String] -> String
text lines =
    -- Note that sentence ends will be followed by an empty string
    let words = splitRegex (mkRegex " ") (concat (intersperse " " lines)) in
    concat . intersperse "\n" . reverse . foldl addword [] $ words
    where
      addword :: [String] -> String -> [String]
      addword [] word = [word]
      addword (line : lines) word =
          if length (line ++ " " ++ word) < 80
          then (line ++ " " ++ word) : lines
          else (word : line : lines)
