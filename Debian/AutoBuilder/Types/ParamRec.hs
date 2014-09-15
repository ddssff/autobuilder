{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Debian.AutoBuilder.Types.ParamRec
    ( ParamRec(..)
    , Strictness(..)
    , TargetSpec(..)
    , prettyPrint
    , getParams
    , usage
    , buildTargets
    ) where

import Control.Arrow (first)
import Data.Generics (listify)
import Data.List as List (map)
import Data.Monoid (mempty, mappend)
import Data.Set as Set (Set, insert, toList)
import Debian.Arch (Arch)
import Debian.AutoBuilder.Types.Packages (Packages(Packages, list, Package), GroupName(GroupName), foldPackages, foldPackages')
import Debian.Pretty (PP(..), ppPrint)
import Debian.Relation (SrcPkgName(SrcPkgName))
import Debian.Release (ReleaseName )
import Debian.Repo.Fingerprint (RetrieveMethod)
import Debian.Repo.Slice (SourcesChangedAction)
import Debian.Sources (DebSource)
import Debian.Version ( DebianVersion, prettyDebianVersion )
import Debian.URI ( URI )
import Prelude hiding (map)
import System.Console.GetOpt
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), text, vcat)
import Text.Read (readMaybe)

-- |An instance of 'ParamClass' contains the configuration parameters
-- for a run of the autobuilder.  Among other things, it defined a set
-- of target packages to build and a single build environment to build
-- them in.  (This typeclass and those belowe were used to transition
-- from a command line option based interface, they could probably be
-- replaced by records now.)  The methods are given in approximate
-- order of importance.
data ParamRec =
    ParamRec
    { vendorTag :: String
    -- ^ The string used to construct modified version numbers to identify
    -- them as part of your repository (rather than Debian's or Ubuntu's.)
    , oldVendorTags :: [String]
    -- ^ Additional vendor tags that should be treated as part of the local
    -- repository, and stripped when deciding the version number of the
    -- upstream source.
    , autobuilderEmail :: String
    -- ^ Email return address of autobuilder for use in generated
    -- changelog entries.
    , releaseSuffixes :: [String]
    -- ^ All build releases must have one of these suffixes.  When the
    -- suffix is stripped off the result is the corresponding base
    -- release.  We use ["-seereason", "-private"] for this value so
    -- we can build a public release based on any debian or ubuntu
    -- release, and a private release based on each public release.
    , buildRelease :: ReleaseName
    -- ^ The name of the release we will be uploading to.  It must end
    -- with one of the 'releaseSuffixes', and stripping off
    -- one of them results in the base release.
    , uploadURI :: Maybe URI
    -- ^ This URI is the address of the remote repository to which packages
    -- will be uploaded after a run with no failures, when the myDoUpload
    -- flag is true.  Packages are uploaded to the directory created by
    -- appending @\/incoming@ to this URI.  This is distinct from the
    -- local repository, where each packages is uploaded immediately after
    -- it is built for use as build dependencies of other packages during
    -- the same run.
    , buildURI :: Maybe URI
    -- ^ An alternate url for the same repository the @uploadURI@ points to,
    -- used for downloading packages that have already been installed
    -- there.
    , targets :: TargetSpec
    -- ^ The packages to build.  The 'Target' record includes the
    -- source package name, a string describing how the source is to
    -- be obtained, and a dependency relaxation list, a list of binary
    -- packages which normally would trigger a rebuild when they
    -- changed.  some methods for obtaining the source code of a
    -- package to be built.  See "Debian.AutoBuilder.BuildTarget" for
    -- information about the available target types.
    , patterns :: [RetrieveMethod]
    -- ^ Targets will be retrieved from the global list based on
    -- whether they contain any of these RetrieveMethod values:
    -- e.g. Hackage "pureMD5"
    , doUpload :: Bool
    -- ^ After a successful build of all the targets, dupload all the
    -- packages in the local pool specified by the @uploadURI@
    -- argument to the corresponding repository.
    , doNewDist :: Bool
    -- ^ After uploading, run newdist on the remote repository
    -- incoming directory
    , flushPool :: Bool
    -- ^ Discard the packages in the local pool before building.  Use
    -- this when a bad package was uploaded to the local pool that
    -- you don't want uploaded to the remote pool.
    , useRepoCache :: Bool
    -- ^ Load the most recent cached repository information from
    -- @~\/.autobuilder\/repoCache@ and assume that it is still good -
    -- that no releases have been added or removed from the
    --,  repositories listed.  This is usually safe and saves some
    -- time querying each remote repository before using it.
    , forceBuild :: [SrcPkgName]
      -- ^,  Build the named source package(s) whether or not they seem
    -- to need it.
    , buildTrumped :: [SrcPkgName]
    -- ^ Build the named source package(s) whether or not they seem
    -- to be older than the version already in the repository.
    , doSSHExport :: Bool
    -- ^ Try to set up ssh keys if upload host asks for a password.
    , report :: Bool
    -- ^ Print a report of packages that are pinned or patched.
    , doHelp :: Bool
    -- ^ Print a usage message and exit.

    -- THINGS THAT ARE OCCASIONALLY USEFUL

    , goals :: [SrcPkgName]
    -- ^ Specify a source package which we want to build, and stop
    -- once all goals are built.  If not given all targets are
    -- considered goals.
    , allowBuildDependencyRegressions :: Bool
    -- ^ Normally, if a build dependency has an older version number
    -- than it did on a previous build, it is an error.  This
    -- generally means the sources.list is incorrect.  However, this
    -- flag can be necessary if a package gets withdrawn from the build
    -- or base release.
    , setEnv :: [(String, Maybe String)]
    -- ^ Set one or more environment variables during the build, e.g. [_$_]
    -- @setEnv = ["DEBIAN_KERNEL_JOBS=5"]@.
    , dryRun :: Bool
    -- ^ This flag says not to do anything that will affect the
    -- outside world, such as uploads and remote newdists.  However,
    -- the files in @~\/.autobuilder@ may still be modified when this
    -- is used.  It does avoids making extensive changes to the
    -- local repository by exiting as soon as a target it identified
    -- as needing to be built.
    , showSources :: Bool
    -- ^ Print the @sources.list@ for the build distro and exit.
    , showParams :: Bool
    -- ^ Print the expanded runtime parameter list and continue.
    , flushAll :: Bool
    -- ^ Remove and re-create the entire autobuilder working directory (topDir.)
    , flushSource :: Bool
    -- ^ Discard and re-download all source code before building.
    , flushDepends :: Bool
    -- ^ Discard all build dependencies by copying the build environment.
    , flushRoot :: Bool
    -- ^ Discard and recreate the clean build environment.
    , verbosity :: Int
    -- ^ Higher numbers increase the amount of progress reporting.
    , topDirParam :: Maybe FilePath
    -- ^ Normally the autobuilder uses @$HOME\/.autobuilder@ for semi-permanent
    -- storage, use this flag to specify a different location.
    , createRelease :: [String]
    -- ^ Pass a @--create <name>@ argument to newdist to create a new
    -- release in the upload repository.
    , doNotChangeVersion :: Bool
    -- ^ DANGER!  Prevents any modification of the package's version
    -- number before building.  Normally a tag is added to signify the
    -- vendor and the base release of the package.  Using this option
    -- can lead to attempts to upload packages that are already
    -- present in the repository, or packages that are trumped by
    -- versions already uploaded to the release.
    , discard :: Set.Set SrcPkgName
    -- ^ When any of these targets become ready to build, fail them.
    -- This is to save time on targets we know will fail.
    , testWithPrivate :: Bool

    -- THINGS THAT RARELY CHANGE

    , sources :: [(String, [DebSource])]
    -- ^ Specify all known @source.list@ files as (name, text) pairs.
    -- The names can be used in apt targets.
    , globalRelaxInfo :: [String]
    -- ^ A list of packages which will not trigger rebuilds when
    -- updated.  Used to avoid massive rebuilds when package which are
    -- build essential but are unlikely to affect the build, such as
    -- @tar@, are updated.
    , strictness :: Strictness
    -- ^ Specify how strict to be about the creation of build
    -- environments, trading off correctness with speed.  In all
    -- cases, a clean build environment is always maintained, and
    -- copied before the package build is performed using @rsync@.
    -- 'Strict' means the clean build environment is discarded and
    -- recreated before each target is built.  'Moderate' means the
    -- clean build environment is kept between successive runs, and
    -- updated as necessary using @apt-get update@ and @apt-get
    -- dist-upgrade@.  'Lax' means that build dependencies are
    -- installed into the clean build environment so that they
    -- accumulate across runs.
    , includePackages :: [String]
    -- ^ Additional packages to include in the clean build environment.
    -- Adding packages here can speed things up when you are building many
    -- packages, because for each package it reverts the build environment
    -- to the clean environment and then installs all the build
    -- dependencies.  This only affects newly created environments, so if
    -- you change this value use the flushRoot option to get it to take
    -- effect.
    , optionalIncludePackages :: [String]
    -- ^ Additional additional packages that might not be immediately
    -- available when a release is created - specifically,
    -- seereason-keyring, which must be built and uploaded to each new
    -- distribution, at least the way we do things.
    , excludePackages :: [String]
    -- ^ Specify packages for build-env to omit from the package list
    -- even if they are marked essential
    , components :: [String]
    -- ^ The list of components of the base repository, for Ubuntu this is
    -- main,restricted,universe,multiverse.
    , developmentReleaseNames :: [String]
    -- ^ The list of upstream release which are currently in
    -- development.  This means we the tag we add doesn't need to
    -- include @~<release>@, since there are no newer releases to
    -- worry about trumping.  Debian's @sid@ is always in this
    -- list, along with the development version of Ubuntu.
    , releaseAliases :: [(String, String)]
    -- ^ Use these aliases for the release name when constructing the
    -- vendor tag used in the version number extension of built
    -- packages.  For example, including the pair @("hardy-seereason",
    -- "hardy")@ here means that packages built for our
    -- @hardy-seereason@ release will be assigned version numbers with
    -- suffixes like @0seereason3~hardy5@ rather than
    -- @0seereason3~hardy-seereason5@ (the latter would be an illegal
    -- due to the dash.)
    , archSet :: Set Arch
    -- ^ The list of architectures to prepare the repository to accept.
    , newDistProgram :: String
    -- ^ Use given executable as the newdist program, the program that
    -- runs on the upload host to install packages in the incoming
    -- directory to the repository.
    , requiredVersion :: [(DebianVersion, Maybe String)]
    -- ^ Specifies the version of the library required.
    , hackageServer :: String
    -- ^ Hostname of the hackage server, normally hackage.haskell.org

    -- THINGS THAT ARE PROBABLY OBSOLETE

    , debug :: Bool
    -- ^ Unspecified debugging behavior.
    , extraReleaseTag :: Maybe Int
    , preferred :: [String]
    -- ^ When selecting build dependencies, prefer this particular
    -- package over other alternatives that could fulfill the
    -- dependency, even if this package seems older than some other
    -- alternative.  For example, the c-compiler virtual package is
    -- provided by @gcc-3.3@, @gcc-3.4@, @gcc-4.0@, etc.  If @gcc-3.4@ is
    -- in this list, a dependency on c-compiler will choose @gcc-3.4@
    -- over the others if possible.
    , buildDepends :: [String]
    -- ^ Obsolete?  Add a missing build dependency.
    , noClean :: Bool
    , cleanUp :: Bool
    -- ^ Do a garbage collection on the local repository, move all
    -- unreferenced files to @removed@.  This is probably not a
    -- useful option, as the local repository is frequently removed.
    , ifSourcesChanged :: SourcesChangedAction
    -- ^ What to do if the sources.list changes in the
    -- configuration directory.  The argument may be
    --
    --  * 'SourcesChangedError' - (the default) print a message and exit, [_$_]
    --
    --  * 'SourcesChangedUpdate' - rewrite sources.list and update the environment, [_$_]
    --
    --  * 'SourcesChangedRemove' - discard and rebuild the environment
    -- , emailTo :: [String]
    -- -- ^ Who should get emails of autobuilder progress messages.
    , buildPackages :: Packages
    -- ^ The set of package that need to be built.
    , knownPackages :: Packages
    -- ^ The set of all known packages
  }

data Strictness
    = Lax |		-- Let build dependencies accumulate (default)
      Strict 		-- Start each target with a clean build environment
      deriving (Eq, Show)

-- |Information about what targets to build are temporarily held in a
-- value of this type.  Once all the command line arguments have been
-- analyzed, this is transformed into a set of targets, which can be
-- used to implement the ParamClass "targets" method.
--
-- We allow some redundancy here by keeping a set of names even while
-- the allTargets flag is set so we can verify that the user never
-- supplies bogus target names.
data TargetSpec
     = TargetSpec
       { allTargets :: Bool
       , groups :: Set.Set GroupName }
     deriving Show

instance Pretty (PP (String, [DebSource])) where
    pPrint (PP (name, ss)) = text $ show $ (ppPrint name, map ppPrint ss)

-- |Output a (somewhat) readable representation of the parameter set.
prettyPrint :: ParamRec -> String
prettyPrint x =
    unlines [ "verbosity=" ++ take 120 (show (verbosity x))
            , "topDirParam=" ++ take 120 (show (topDirParam x))
            , "debug=" ++ take 120 (show (debug x))
            , "dryRun=" ++ take 120 (show (dryRun x))
            , "requiredVersion=" ++ take 120 (show (map (first prettyDebianVersion) (requiredVersion x)))
            , "showSources=" ++ take 120 (show (showSources x))
            , "showParams=" ++ take 120 (show (showParams x))
            , "flushAll=" ++ take 120 (show (flushAll x))
            , "useRepoCache=" ++ take 120 (show (useRepoCache x))
            , "sources=" ++ take 120 (show (vcat (map ppPrint (sources x))))
            , "targets=" ++ take 120 (show (targets x))
            , "patterns=" ++ take 120 (show (patterns x))
            , "goals=" ++ take 120 (show (goals x))
            , "discard=" ++ take 120 (show (discard x))
            , "vendorTag=" ++ take 120 (show (vendorTag x))
            , "oldVendorTags=" ++ take 120 (show (oldVendorTags x))
            , "extraReleaseTag=" ++ take 120 (show (extraReleaseTag x))
            , "flushSource=" ++ take 120 (show (flushSource x))
            , "flushDepends=" ++ take 120 (show (flushDepends x))
            , "forceBuild=" ++ take 120 (show (forceBuild x))
            , "buildTrumped=" ++ take 120 (show (buildTrumped x))
            , "allowBuildDependencyRegressions=" ++ take 120 (show (allowBuildDependencyRegressions x))
            , "preferred=" ++ take 120 (show (preferred x))
            , "strictness=" ++ take 120 (show (strictness x))
            , "setEnv=" ++ take 120 (show (setEnv x))
            , "buildDepends=" ++ take 120 (show (buildDepends x))
            , "globalRelaxInfo=" ++ take 120 (show (globalRelaxInfo x))
            , "noClean=" ++ take 120 (show (noClean x))
            , "includePackages=" ++ take 120 (show (includePackages x))
            , "optionalIncludePackages=" ++ take 120 (show (optionalIncludePackages x))
            , "excludePackages=" ++ take 120 (show (excludePackages x))
            , "components=" ++ take 120 (show (components x))
            , "buildRelease=" ++ take 120 (show (buildRelease x))
            , "releaseSuffixes=" ++ take 120 (show (releaseSuffixes x))
            , "developmentReleaseNames=" ++ take 120 (show (developmentReleaseNames x))
            , "doNotChangeVersion=" ++ take 120 (show (doNotChangeVersion x))
            , "releaseAliases=" ++ take 120 (show (releaseAliases x))
            , "flushRoot=" ++ take 120 (show (flushRoot x))
            , "cleanUp=" ++ take 120 (show (cleanUp x))
            , "archSet" ++ take 120 (show (archSet x))
            , "flushPool=" ++ take 120 (show (flushPool x))
            , "doUpload=" ++ take 120 (show (doUpload x))
            , "doNewDist=" ++ take 120 (show (doNewDist x))
            , "newDistProgram=" ++ take 120 (show (newDistProgram x))
            , "uploadURI=" ++ take 120 (show (uploadURI x))
            , "buildURI=" ++ take 120 (show (buildURI x))
            , "createRelease=" ++ take 120 (show (createRelease x))
            , "hackageServer=" ++ take 120 (show (hackageServer x))
            --, "ifSourcesChanged=" ++ take 120 (show (ifSourcesChanged x))
            , "doSSHExport=" ++ take 120 (show (doSSHExport x))
            , "autobuilderEmail=" ++ take 120 (show (autobuilderEmail x))
            --, "baseRelease sources=\n" ++ show (lookup (sliceName (baseRelease x)) (sources x))
            ]

-- |A Left String value indicates that beginning of a new autobuilder
-- run for the named release.  Following that are Right (ParamRec ->
-- ParamRec) values which transform the parameter record that
-- describes that autobuilder run.
optSpecs :: [OptDescr (Either String (ParamRec -> ParamRec))]
optSpecs =
    [ Option [] ["top"] (ReqArg (\ s -> Right (\ p -> p {topDirParam = Just s})) "PATH")
      "Set the top directory, normally ${HOME}/.autobuilder"
    , Option ['v'] ["verbose"] (NoArg (Right (\ p -> p {verbosity = verbosity p + 1})))
      "Increase progress reporting.  Can be used multiple times."
    , Option ['q'] ["quiet"] (NoArg (Right (\ p -> p {verbosity = verbosity p - 1})))
      "Decrease progress reporting. Can be used multiple times."
    , Option [] ["show-params"] (NoArg (Right (\ p -> p {showParams = True})))
      "Display the parameter set"
    , Option [] ["flush-repo-cache"] (NoArg (Right (\ p -> p {useRepoCache = False})))
      (unlines [ "Ignore the existing cached information about the remote repositories,"
               , "instead rebuild it from scratch and save the new result" ])
    , Option [] ["flush-pool"] (NoArg (Right (\ p -> p {flushPool = True})))
      (unlines [ "Flush the local repository before building.  It is probably a good"
               , "idea to also use --flush-depends, the packages being flushed from the"
               , "local pool might have also been installed into the build environment"
               , "to satisfy build dependencies." ])
    , Option [] ["flush-root"] (NoArg (Right (\ p -> p {flushRoot = True})))
      "Discard and recreate the clean and build environments."
    , Option [] ["flush-source"] (NoArg (Right (\ p -> p {flushSource = True})))
      "Discard and re-download all source code."
    , Option [] ["flush-depends"] (NoArg (Right (\ p -> p {flushPool = True, flushDepends = True})))
      "Flush all the installed build dependencies from the build environment."
    , Option [] ["flush-all"] (NoArg (Right (\ p -> p {flushAll = True})))
      "Remove and re-create the entire autobuilder working directory."
    , Option [] ["do-upload"] (NoArg (Right (\ p -> p {doUpload = True})))
      "Upload the packages to the remote server after a successful build."
    , Option [] ["do-newdist"] (NoArg (Right (\ p -> p {doNewDist = True})))
      "Run newdist on the remote server after a successful build and upload."
    , Option ['n'] ["dry-run"] (NoArg (Right (\ p -> p {dryRun = True})))
      "Exit as soon as we discover a package that needs to be built."
    , Option [] ["all-targets"] (NoArg (Right (\ p ->  p {targets = (targets p) {allTargets = True}})))
      "Add all known targets for the release to the target list."
    , Option [] ["allow-build-dependency-regressions"]
                 (NoArg (Right (\ p -> p {allowBuildDependencyRegressions = True})))
      (unlines [ "Normally it is an error if a build dependency has an older version"
               , "number than during the previous looks older than it did during the"
               , "previous build.  This option relaxes that assumption, in case the"
               , "newer version of the dependency was withdrawn from the repository,"
               , "or was flushed from the local repository without being uploaded."])
    , Option [] ["target", "group"] (ReqArg (\ s -> (Right (\ p -> p {targets = addTarget (GroupName s) p}))) "GROUP NAME")
      "Add a target to the target list."
    , Option [] ["pattern"] (ReqArg (\ s -> (Right (\ p -> p {patterns = readPattern s ++ patterns p}))) "EXPRESSION")
      "Find targets by pattern matching"
    , Option [] ["discard"] (ReqArg (\ s -> (Right (\ p -> p {discard = Set.insert (SrcPkgName s) (discard p)}))) "PACKAGE")
      (unlines [ "Add a target to the discard list, packages which we discard as soon"
               , "as they are ready to build, along with any packages that depend on them." ])
    , Option [] ["test-with-private"] (NoArg (Right (\ p -> p {testWithPrivate = True})))
      (unlines [ "Build everything required to build the private targets, but don't"
               , "actually build the private targets.  This is to avoid the risk of"
               , "uploading private targets to the public repository" ])
    , Option [] ["goal"] (ReqArg (\ s -> (Right (\ p -> p { goals = goals p ++ [SrcPkgName s]
                                                          -- , targets = TargetSet (myTargets home (const True) (relName (buildRelease p)))
                                                          }))) "PACKAGE")
      (unlines [ "If one or more goal package names are given the autobuilder"
               , "will only build these packages and any of their build dependencies"
               , "which are in the package list.  If no goals are specified, all the"
               , "targets will be built.  (As of version 5.2 there are known bugs with"
               , "this this option which may cause the autobuilder to exit before the"
               , "goal package is built.)"])
    , Option [] ["force"] (ReqArg (\ s -> (Right (\ p -> p {forceBuild = forceBuild p ++ [SrcPkgName s]}))) "PACKAGE")
      ("Build the specified source package even if it doesn't seem to need it.")
    , Option [] ["strict"] (NoArg (Right (\ p -> p {strictness = Strict})))
      "Use the lax build environment, where dependencies are not removed between package builds."
    , Option [] ["build-trumped"] (ReqArg (\ s -> (Right (\ p -> p {buildTrumped = buildTrumped p ++ [SrcPkgName s]}))) "PACKAGE")
      ("Build the specified source package even if it seems older than the uploaded version.")
    , Option [] ["report"] (NoArg (Right (\ p -> p {report = True})))
      "Output a report of packages that are pinned or patched."
    , Option ['h'] ["help", "usage"] (NoArg (Right (\ p -> p {doHelp = True})))
      "Print a help message and exit."
    ]
    where
      addTarget s p = (targets p) {groups = Set.insert s (groups (targets p))}
{-
      allTargets p =
          p {targets = let name = (relName (buildRelease p)) in TargetList (myTargets (releaseTargetNamePred name) name)})
      ++ [find s p]
      find s p = case filter (\ t -> sourcePackageName t == s) (myTargets (const True) (relName (buildRelease p))) of
                   [x] -> x
                   [] -> error $ "Package not found: " ++ s
                   xs -> error $ "Multiple packages found: " ++ show (map sourcePackageName xs)
-}

readPattern :: Monad m => String -> m RetrieveMethod
readPattern s = maybe (fail $ "Invalid RetrieveMethod: " ++ show s) return (readMaybe s)

-- |given a list of strings as they would be returned from getArgs,
-- build the list of ParamRec which defines the build.
-- 
-- Example: getParams ["lucid-seereason" "--all-targets"] >>= return . map buildRelease
--            -> [ReleaseName {relName = "lucid-seereason"}]
getParams :: [String] -> (String -> ParamRec) -> [ParamRec]
getParams args params =
    doParams (getOpt' (ReturnInOrder Left) optSpecs args)
    where
      -- Turn the parameter information into a list of parameter records
      -- containing all the info needed during runtime.  Each return record
      -- represents a separate autobuilder run.
      doParams :: ([Either String (ParamRec -> ParamRec)], -- The list of functions to apply to the default record
                   [String],               -- non-options
                   [String],               -- unrecognized options
                   [String])               -- error messages
               -> [ParamRec]
      doParams (fns, [], [], []) =
          reverse $ f [] fns
          where
            f recs [] = recs
            f recs (Left rel : xs) = f (params rel : recs) xs
            f (rec : more) (Right fn : xs) = f (fn rec : more) xs
            f _ _ = error "First argument must be a release name"
      doParams (_, _, badopts, errs) =
          error (usage ("Bad options: " ++ show badopts ++ ", errors: " ++ show errs))

-- Intermediate type used while formatting the usage info.
data DescrLine
    = Opt { long :: String, short :: String }
    | Text String

-- |Modified version of System.Console.GetOpt.usageInfo, avoids printing
-- such wide lines.
usage :: String		-- header
          -> String		-- nicely formatted decription of options
usage header =
    unlines (header:table)
    where table = map fmtLine xs
          fmtLine (Text s) = "    " ++ s
          fmtLine (Opt {long = lopts, short = sopts}) =
              "  " ++
              flushLeft lsl lopts ++ "  " ++
              flushLeft ssl sopts
          xs = legend ++ concatMap fmtOpt optSpecs
          ssl = foldl max 0 (map ss xs)
          lsl = foldl max 0 (map ls xs)
          -- The length of a short option
          ss opt@(Opt _ _) = length (short opt)
          ss _ = 0
          -- The length of a long option
          ls opt@(Opt _ _) = length (long opt)
          ls _ = 0
          flushLeft n x = take n (x ++ repeat ' ')
          legend = [Opt {long = "Long option", short = "Short option"},
                    Opt {long = "-----------", short = "------------"}]

fmtOpt :: OptDescr a -> [DescrLine]
fmtOpt (Option sos los ad descr) =
   let ds = [Text ""] ++ map Text (lines descr) ++ [Text ""]
       ss = map (fmtShort ad) sos
       ls = map (fmtLong  ad) los in
   let n = max (length ls) (length ss) in
   let ss' = ss ++ replicate (n - length ss) ""
       ls' = ls ++ replicate (n - length ls) "" in
       -- ps' = ps ++ replicate (n - length ps) "" in
   map (\ (l, s) -> Opt {long = l, short = s}) (zip ls' ss') ++ ds

fmtShort :: ArgDescr a -> Char -> String
fmtShort (NoArg  _   ) so = "-" ++ [so]
fmtShort (ReqArg _ ad) so = "-" ++ [so] ++ " " ++ ad
fmtShort (OptArg _ ad) so = "-" ++ [so] ++ "[" ++ ad ++ "]"

fmtLong :: ArgDescr a -> String -> String
fmtLong (NoArg  _   ) lo = "--" ++ lo
fmtLong (ReqArg _ ad) lo = "--" ++ lo ++ "=" ++ ad
fmtLong (OptArg _ ad) lo = "--" ++ lo ++ "[=" ++ ad ++ "]"

buildTargets :: ParamRec -> Packages -> Packages
buildTargets params knownTargets =
    case targets params of
      TargetSpec {allTargets = True} -> knownTargets
      TargetSpec {groups = names} -> Packages {list = map findByName (toList names) ++ concatMap findByPattern (patterns params)}
    where
      findByName :: GroupName -> Packages
      findByName s =
          foldPackages' f n g h knownTargets mempty
          where
            f _ _ r = r
            n s' p r = if s == s'
                       then mappend r p
                       else foldPackages' f n g h p r
            g ps r = foldr (foldPackages' f n g h) r ps
            h r = r
      -- Filter the singleton packages by whether its RetrieveMethod
      -- contains pat.
      findByPattern  :: RetrieveMethod -> [Packages]
      findByPattern pat = foldPackages (\ method flags r -> case listify (== pat) method of
                                                              [] -> r
                                                              _ -> Package method flags : r) knownTargets []
