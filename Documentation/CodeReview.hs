
data SrcDeb = SrcDeb 

data DistributionSpec = DistributionSpec [SrcDebInfo]
data Distribution = Distribution Location [Architecture] [(SrcDebInfo, [BinDebs]

OriginalDistribution :: DistSpec
buildTargets :: [SrcDeb]

Compute new dist >>= print

buildTargets union with ([ p | p <- OriginalDistribution, isBuildDep p (something in buildTargets)])

-- if compilation of a package is omitted by the developer, then review for runtime dependency checks.

[ (p, buildDep p) | p <- buildTargets,


getNextVersion :: Refinery -> SrcDeb -> IO DebVersion
getNextVersion :: DistSpec -> SrcDeb -> DebVersion


reifySrcDeb :: TargetSpec ... -> SrcDeb

sourcesListToDistSpec :: SourcesList -> IO DistSpec
getDistSpecByNameFromRefinery :: Refinery -> DistName -> IO DistSpec

partialDist :: [ SrcDeb ] -> DistSpec
mergeDistSpec :: DistSpec -> DistSpec -> DistSpec





{- Version number/changelog problem really complicated.
   How do we know if imported from Debian cleanly?
   developer might have changed code, but not updated the changelog
   or the reverse, changelog but no changes (is that okay? equivalent of force version increment?)

   TODO We need to write up methods/rules about deciding whether the changelog is new, etc.
 -}
   

case changeLog of
  developer has manually updated changelog, bumping version
  developer has manually updated changelog, not bumping version
  

ChangeLogPath -> [Changelog]

data ChangeLog = ChangeLog (version, date, comment)



****************************************************************

[TargetSpec] -> [DistSpec]


autobuilderDryRun :: Distribution -> [TargetSpec] -> IO Either BigExplanation (DistSpecDelta)
autobuilderFullRun :: Distribution -> [TargetSpec] -> IO Either BigExplanation (DistSpecDelta,Distribution, DistributionSpec)

-- DistSpecDelta is best effort of dry run, version numbers, list of binary debs may be incorrect if package is debian wacky.

-- perhaps DistSpec should be extended to include the OrderingHints?
-- may not be possible to complete ordering initially, so this operation may have to be lazy, i.e., lop off first part, then you have to recompute later
buildOrdering :: DistSpec -> OrderingHints -> [SrcDeb] -> Either BigExplanation [DistSpec] -- list of DistSpecs in order that they should be realized.
buildOrderingInteractive :: DistSpec -> [SrcDeb] -> IO [DistSpec]


mkPackageCache :: [Distribution] -> PackageCache -- ??? is a package cache just a distribution?

-- A package is either already in the cache or it is guaranteed that all its build dependencies are in the package cache.
-- Any package in the DistSpec that needs to be built must be able to be built directly from build dependencies in the package cache.
-- N.B.  that means that all missing packages may be built in parallel.
makeDistribution :: PackageCache -> DistSpec -> IO Either BigExplanation Distribution  -- calls package build, etc.

-- potential failure, if new package A builds, but resulting version is not higher than old version, next one will fail.



satisfyBuildDep :: Distribution -> BuildDependencySpec -> IO [BinDeb]

`-- circular dependencies?

essentialPackages :: Distribution -> IO [BinDeb]
dependenciesOf :: Distribution -> BinDeb -> IO [BinDeb]
existingBuildEnv :: Distribution -> [BinDeb] -> IO BuildEnv
newBuildEnv :: Distribution -> SrcDeb -> IO BuildEnv
packageBuild :: BuildEnv -> SrcDeb -> IO [BinDeb]



diffDist :: DistributionSpec -> DistributionSpec -> DistSpecDelta deriving HumanReadableReport


-- Remaining questions:
--   if PackageCache == Distribution, the makeDistribution can be folded over [DistSpec] which is the output of buildOrdering.



JS -- 2006/09/05

{- Calculating things to be rebuilt by tracing build dependencies runs into problems of missing information.
   Assume package A `BuildDepends` B, B `BuildDepends` C, user changes both A and B.

 -}

-- TODO -- Compare with JS code in Debian.*,  Unify, regularize and standardize.

type PackageName = String

data VersionBound = VersionBound (Comparison,DebianVersion)
data DebianPackageConstraint = Package PackageName (Maybe VersionBound) (Maybe ArchitectureDep)

data DebianRelation = BuildDepends [DebianPackageConstraint]



data ArchitectureDep = OnlyArchitectures [Architecture] | ExceptArchitectures [Architecture]
type VersionDep = DebianPackageConstraint

data PackageDep = Package Name, Maybe VersionDep, Maybe ArchitectureDep | Alternatives [Package]


data BuildDependencySpec = BDS (Maybe Op, [(PackageName, Maybe VersionClause, Maybe ArchitectureSpec)])
data BuildConflictSpec = BCS (PackageName, Maybe VersionClause, Maybe ArchitectureSpec)
data BuildDependencySpecIndep = BDSI (PackageName, Maybe VersionClause, Maybe ArchitectureSpec)
data BuildConflictSpecIndep = BCSI (PackageName, Maybe VersionClause, Maybe ArchitectureSpec)


data SrcDebInfo = SrcDebInfo { srcDebName :: PackageName
                             , srcVersion:: DebianVersion
                             , binDebs :: Expected [(BinaryDebNames,DebianVersion)]
                             , buildDeps :: [BuildDependencySpec]
                             }

data DistributionSpec = DistributionSpec [SrcDebInfo]


finalVersionCheck :: ExpectedVersion -> GeneratedVersion -> Bool


data QAStatus = Stable | Testing | Unstable

-- newtype = subtype String with {[a-f0-9]}25
newtype CheckSumStringMD5Sum = MD5Sum String
newtype CheckSumStringSHA1Sum = SHA1Sum String
newtype CheckSumStringSHA256Sum = SHA256Sum String

data CheckSum = { md5sum :: Maybe CheckSumStringMD5Sum
                , sha1sum :: Maybe CheckSumStringSHA1Sum
                , sha256sum :: Maybe CheckSumStringSHA256Sum
                }

data DistributionRelease = DistributionRelease { origin :: String
                                               , label :: String
                                               , suite :: QAStatus
                                               , codename :: String
                                               , timestamp :: Date
                                               , archs :: [ ArchitectureSpec ]
                                               , components :: [Strings]
                                               , description :: String
                                               , indices :: [(FilePath, Size, [CheckSum])]


-- Head of release file

--    Origin: Debian
--    Label: Debian
--    Suite: testing
--    Codename: etch
--    Date: Thu, 31 Aug 2006 20:24:39 UTC
--    Architectures: alpha amd64 arm hppa i386 ia64 m68k mips mipsel powerpc s390 sparc
--    Components: main contrib non-free
--    Description: Debian Testing distribution - Not Released
--    MD5Sum:
--     c2edf6acb4dcc067afc010aa0a2cad40 17779997 main/binary-alpha/Packages

---


-- UI level operations, describing workflow, many of which are expected to be separated out from autobuilder core.

-- Creating new distribution specifications

-- Update a distspec with my local package changes
-- Update a distspec with new packages from some other distribution (e.g., get latest from sid)
-- Update distspec with diff of two others.
-- Use autobuilder algorithm to track propagation of build dependency changes.

-- Examples:

makeNewDistributionSpec :: Maybe [ TargetSpec ] -> DistributionSpec
mergeDistributionSpec :: MergeOperator -> DistributionSpec -> DistributionSpec -> DistributionSpec


-- TODO store .changes .dsc


-- generate a user readable report about changes in source debs, patches, etc.  Place holder for extensive system.
reportChanges :: DistributionSpec -> DistributionSpec -> String | XML

-- MergeOperator gives priority to either left, right or highest version, possibly with info about dependencies.
-- Experiment with requirements.

autobuilderDryRunReport :: Distribution -> DistributionSpec -> Maybe BuildHints -> IO (String, Maybe BuildHints, Maybe DistributionSpec)

autobuilder :: Distribution -> DistributionSpec -> Maybe BuildHints -> IO Distribution


-- Distribution validation
-- make Contents and check for conflicts
-- Dependency analysis
-- upgradability -> check that all packages that changed versions install/remove.  More?
-- script correctness





packageBuild :: Distribution -> SrcDeb -> IO (ChangesFile, [BinDeb])

binaryPackageContentsChanges :: BinDeb -> BinDeb -> IO Bool -- True if any file other than control and changelog has changed md5sum.
                                                            -- File addition/deletion => True as well


reversion :: SrcDeb -> IO SrcDeb

data BinDeb = BinDeb ... [BinaryBuildDependencies]

checksum $ checksum autobuilder ++ map checksum [BinDeb] ++ checksum SrcDeb



autobuild :: Distribution -> [SrcDeb] -> IO [(SrcDebChanges, SrcDeb, [BinDeb])]
autobuild dist src:srcs =
    do env <- newBuildEnv dist src
       (changes,bindebs) <- packageBuild env src
       return ((changes,src,bindebs) : autobuild (merge dist (src, bindebs)) srcs)


autobuild :: Distribution -> [SrcDeb] -> IO [(SrcDebChanges, SrcDeb, [BinDeb])]
autobuild dist srcs =
    let (src,srcs) = pickNext srcs in
    do env <- newBuildEnv dist src
       (changes,bindebs) <- packageBuild env src
       return ((changes,src,bindebs) : autobuild (merge dist (src, bindebs)) srcs)

autobuild :: DistributionSpec -> Distribution -> [SrcDeb] -> IO [(SrcDebChanges, SrcDeb, [BinDeb])]
autobuild distspec dist srcs =
    let smartRebuildFilter = rebuildOverride buildDeps listOfAllPackagesAlreadyBuilt userSpecifiedAnswers
        (src,srcs) = findPackageWithMinBuildDependency smartRebuildFilter srcs in
    -- for parallelism, pick all packages with minimum score
    do env <- newBuildEnv dist src
       (changes,bindebs) <- packageBuild env src
       (newDist, distDelta) <- merge distspec dist (src,bindebs)
       brokenDepSourceDebs <- unique $ map sourceOf $ findNewlyBrokenDependencies newDist distDelta
       changedBuildDependencySourceDebs <- map sourceOf $ findPackagesWithNewlyModifiedBuildDependencies newDist distDelta
       return ((changes,src,bindebs) : autobuild newDist (foldr1 union [srcs, brokenDepSourceDebs,changedBuildDependencySourceDebs])

-- merge distspec dist (src,[bindeb])
-- distspec is used to determine if the old source deb should be removed.


rebuildOverride buildDeps listOfAllPackagesAlreadyBuilt userSpecifiedAnswers src =
    foldl (||) False assertions
        where assertions = [ circularBuildDepDetected listOfAllPackagesAlreadyBuilt
                           , cacheHitDetected -- already in pool: checksum $ checksum autobuilder ++ map checksum [BinDeb] ++ checksum SrcDeb 
                           , all $ packageContentsUnchangedAfterRebuild buildDeps
                           , humanKnowsBest src userSpecifiedAnswers
                           ]

-- Jeremy's thoughts on build dependency regeneration
-- if (A, C in changedSourceDebs) and (A `buildDepends` B) and (B `buildDepends` C) then rebuild B
-- However, if (Z `buildDepends` A) and (not Z in changedSourceDebs) then rebuild only in long term build.

-- Exceptions:  If versionBump A.a1 breaks binary package constraints of Z.z2, then  you must rebuild A.

-- CB: alternatively
-- while working on a small set of packages:
---- makeNewDist "working" (let ps = [packages] ; isops = [isopackages] in union ps (map buildDeps ps) (map binaryDeps ps) (map binaryDeps isops) isops)



deb://sandbox.linspire.com/dists/freespire/debs/emacs21/DEBIAN/control
deb-src://sandbox.linspire.com/dists/freespire/debs/emacs21/DEBIAN/control



-- TODO
-- Consider logging, history, garbage collection and email/rss notification

-- Request from DSF, optimize time to decide that no changes are required.