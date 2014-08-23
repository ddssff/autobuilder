module Test.Fingerprint where

import Test.HUnit
import Data.List (intercalate)
import Data.Set (fromList)
import Debian.AutoBuilder.Types.Fingerprint
import Debian.AutoBuilder.Types.Packages (RetrieveMethod(..), RetrieveAttribute(..), GitSpec(..))
import Debian.Relation
import Debian.Repo.PackageID (PackageID(..))
import Debian.Version
import Distribution.Package ()

tests =
    TestList
    [ TestCase $ assertEqual "Fingerprint show test 1" "\"Dir \\\"/tmp/foo\\\"\" [] 1.0 ghc=7.8.20140710-2+seereason1~precise1" (showFingerprint fingerprint0a)
    , TestCase $ assertEqual "Fingerprint show test 1" "\"Dir \\\"/tmp/foo\\\"\" [GitCommit \"520591876ee57dbecba1b2de602dc79f4f67ecce\", GitCommit \"907be0999fb8801d9560f90f39fb2ef6f8ef7c6b\"] 1.0 ghc=7.8.20140710-2+seereason1~precise1" (showFingerprint fingerprint0b)
    , TestCase $ assertEqual "Fingerprint round trip test 1" (Just fingerprint0a) (readUpstreamFingerprint (showFingerprint fingerprint0a))
    , let method = "\"Proc (DebDir (Cd \\\"Cabal\\\" (Git \\\"https://github.com/ghcjs/cabal\\\" [Branch \\\"ghcjs\\\"])) (Git \\\"https://github.com/ddssff/cabal-ghcjs-debian\\\" []))\""
          attrs = "[]"
          version = "1.21.0.0-2"
          builddeps = "base-files=6.5ubuntu6.8 base-passwd=3.5.24 bash=4.2-2ubuntu2.1 bsdutils=1:2.20.1-1ubuntu3.1 cdbs=0.4.100ubuntu2 coreutils=8.13-3ubuntu3.2 dash=0.5.7-2ubuntu2 debhelper=9.20120115ubuntu3 debianutils=4.2.1ubuntu2 diffutils=1:3.2-1ubuntu1 dpkg=1.16.1.2ubuntu7.5 dpkg-dev=1.16.1.2ubuntu7.5 e2fsprogs=1.42-1ubuntu2 findutils=4.4.2-4ubuntu1 g++=4:4.6.3-1ubuntu5 gcc=4:4.6.3-1ubuntu5 ghc=7.8.20140710-2+seereason1~precise1 ghc-doc=7.8.20140710-2+seereason1~precise1 ghc-prof=7.8.20140710-2+seereason1~precise1 grep=2.10-1 gzip=1.4-1ubuntu2 haskell-devscripts=0.8.21.3-0+seereason1~precise1 hostname=3.06ubuntu1 libc6-dev=2.15-0ubuntu10.6 login=1:4.1.4.2+svn3283-3ubuntu5.1 make=3.81-8.1ubuntu1.1 mount=2.20.1-1ubuntu3.1 ncurses-base=5.9-4 ncurses-bin=5.9-4 perl-base=5.14.2-6ubuntu2.4 python-minimal=2.7.3-0ubuntu2.2 sed=4.2.1-9 tar=1.26-4ubuntu1 util-linux=2.20.1-1ubuntu3.1"
      in
      TestCase $ assertEqual "Fingerprint Test 2" (Just fingerprint1) (readUpstreamFingerprint (intercalate " " [method, attrs, version, builddeps]))
    , TestCase $ assertEqual "Fingerprint Test 3" (Just fingerprint1) (readUpstreamFingerprint "\"Proc (DebDir (Cd \\\"Cabal\\\" (Git \\\"https://github.com/ghcjs/cabal\\\" [Branch \\\"ghcjs\\\"])) (Git \\\"https://github.com/ddssff/cabal-ghcjs-debian\\\" []))\" 1.21.0.0-2 base-files=6.5ubuntu6.8 base-passwd=3.5.24 bash=4.2-2ubuntu2.1 bsdutils=1:2.20.1-1ubuntu3.1 cdbs=0.4.100ubuntu2 coreutils=8.13-3ubuntu3.2 dash=0.5.7-2ubuntu2 debhelper=9.20120115ubuntu3 debianutils=4.2.1ubuntu2 diffutils=1:3.2-1ubuntu1 dpkg=1.16.1.2ubuntu7.5 dpkg-dev=1.16.1.2ubuntu7.5 e2fsprogs=1.42-1ubuntu2 findutils=4.4.2-4ubuntu1 g++=4:4.6.3-1ubuntu5 gcc=4:4.6.3-1ubuntu5 ghc=7.8.20140710-2+seereason1~precise1 ghc-doc=7.8.20140710-2+seereason1~precise1 ghc-prof=7.8.20140710-2+seereason1~precise1 grep=2.10-1 gzip=1.4-1ubuntu2 haskell-devscripts=0.8.21.3-0+seereason1~precise1 hostname=3.06ubuntu1 libc6-dev=2.15-0ubuntu10.6 login=1:4.1.4.2+svn3283-3ubuntu5.1 make=3.81-8.1ubuntu1.1 mount=2.20.1-1ubuntu3.1 ncurses-base=5.9-4 ncurses-bin=5.9-4 perl-base=5.14.2-6ubuntu2.4 python-minimal=2.7.3-0ubuntu2.2 sed=4.2.1-9 tar=1.26-4ubuntu1 util-linux=2.20.1-1ubuntu3.1")
    ]

fingerprint0a =
    Fingerprint
    { method = Dir "/tmp/foo"
    , retrievedAttributes = Data.Set.fromList []
    , upstreamVersion = parseDebianVersion ("1.0" :: String)
    , buildDependencyVersions =
        Data.Set.fromList [PackageID {packageName = BinPkgName {unBinPkgName = "ghc"},
                                      packageVersion = (parseDebianVersion ("7.8.20140710-2+seereason1~precise1" :: String))}] }

fingerprint0b =
    Fingerprint
    { method = Dir "/tmp/foo"
    , upstreamVersion = parseDebianVersion ("1.0" :: String)
    , retrievedAttributes = Data.Set.fromList [GitCommit "520591876ee57dbecba1b2de602dc79f4f67ecce",
                                               GitCommit "907be0999fb8801d9560f90f39fb2ef6f8ef7c6b"]
    , buildDependencyVersions = Data.Set.fromList [PackageID {packageName = BinPkgName {unBinPkgName = "ghc"},
                                                              packageVersion = (parseDebianVersion ("7.8.20140710-2+seereason1~precise1" :: String))}] }
-- no retrieve attributes

fingerprint1 =
    Fingerprint
    { method = cabalMethod
    , upstreamVersion = parseDebianVersion ("1.21.0.0-2" :: String)
    , retrievedAttributes = Data.Set.fromList []
    , buildDependencyVersions = Data.Set.fromList deps }

fingerprint2 =
    Fingerprint
    { method = cabalMethod
    , upstreamVersion = parseDebianVersion ("1.21.0.0-2" :: String)
    , retrievedAttributes = Data.Set.fromList attrs
    , buildDependencyVersions = Data.Set.fromList deps }

cabalMethod =
    Proc (DebDir (Cd "Cabal" (Git "https://github.com/ghcjs/cabal" [Branch "ghcjs"]))
                 (Git "https://github.com/ddssff/cabal-ghcjs-debian" []))

attrs = [GitCommit "520591876ee57dbecba1b2de602dc79f4f67ecce",GitCommit "907be0999fb8801d9560f90f39fb2ef6f8ef7c6b"]

deps = [PackageID {packageName = BinPkgName {unBinPkgName = "base-files"}, packageVersion = (Debian.Version.parseDebianVersion ("6.5ubuntu6.8" :: String))}
       ,PackageID {packageName = BinPkgName {unBinPkgName = "base-passwd"}, packageVersion = (Debian.Version.parseDebianVersion ("3.5.24" :: String))}
       ,PackageID {packageName = BinPkgName {unBinPkgName = "bash"}, packageVersion = (Debian.Version.parseDebianVersion ("4.2-2ubuntu2.1" :: String))}
       ,PackageID {packageName = BinPkgName {unBinPkgName = "bsdutils"}, packageVersion = (Debian.Version.parseDebianVersion ("1:2.20.1-1ubuntu3.1" :: String))}
       ,PackageID {packageName = BinPkgName {unBinPkgName = "cdbs"}, packageVersion = (Debian.Version.parseDebianVersion ("0.4.100ubuntu2" :: String))}
       ,PackageID {packageName = BinPkgName {unBinPkgName = "coreutils"}, packageVersion = (Debian.Version.parseDebianVersion ("8.13-3ubuntu3.2" :: String))}
       ,PackageID {packageName = BinPkgName {unBinPkgName = "dash"}, packageVersion = (Debian.Version.parseDebianVersion ("0.5.7-2ubuntu2" :: String))}
       ,PackageID {packageName = BinPkgName {unBinPkgName = "debhelper"}, packageVersion = (Debian.Version.parseDebianVersion ("9.20120115ubuntu3" :: String))}
       ,PackageID {packageName = BinPkgName {unBinPkgName = "debianutils"}, packageVersion = (Debian.Version.parseDebianVersion ("4.2.1ubuntu2" :: String))}
       ,PackageID {packageName = BinPkgName {unBinPkgName = "diffutils"}, packageVersion = (Debian.Version.parseDebianVersion ("1:3.2-1ubuntu1" :: String))}
       ,PackageID {packageName = BinPkgName {unBinPkgName = "dpkg"}, packageVersion = (Debian.Version.parseDebianVersion ("1.16.1.2ubuntu7.5" :: String))}
       ,PackageID {packageName = BinPkgName {unBinPkgName = "dpkg-dev"}, packageVersion = (Debian.Version.parseDebianVersion ("1.16.1.2ubuntu7.5" :: String))}
       ,PackageID {packageName = BinPkgName {unBinPkgName = "e2fsprogs"}, packageVersion = (Debian.Version.parseDebianVersion ("1.42-1ubuntu2" :: String))}
       ,PackageID {packageName = BinPkgName {unBinPkgName = "findutils"}, packageVersion = (Debian.Version.parseDebianVersion ("4.4.2-4ubuntu1" :: String))}
       ,PackageID {packageName = BinPkgName {unBinPkgName = "g++"}, packageVersion = (Debian.Version.parseDebianVersion ("4:4.6.3-1ubuntu5" :: String))}
       ,PackageID {packageName = BinPkgName {unBinPkgName = "gcc"}, packageVersion = (Debian.Version.parseDebianVersion ("4:4.6.3-1ubuntu5" :: String))}
       ,PackageID {packageName = BinPkgName {unBinPkgName = "ghc"}, packageVersion = (Debian.Version.parseDebianVersion ("7.8.20140710-2+seereason1~precise1" :: String))}
       ,PackageID {packageName = BinPkgName {unBinPkgName = "ghc-doc"}, packageVersion = (Debian.Version.parseDebianVersion ("7.8.20140710-2+seereason1~precise1" :: String))}
       ,PackageID {packageName = BinPkgName {unBinPkgName = "ghc-prof"}, packageVersion = (Debian.Version.parseDebianVersion ("7.8.20140710-2+seereason1~precise1" :: String))}
       ,PackageID {packageName = BinPkgName {unBinPkgName = "grep"}, packageVersion = (Debian.Version.parseDebianVersion ("2.10-1" :: String))}
       ,PackageID {packageName = BinPkgName {unBinPkgName = "gzip"}, packageVersion = (Debian.Version.parseDebianVersion ("1.4-1ubuntu2" :: String))}
       ,PackageID {packageName = BinPkgName {unBinPkgName = "haskell-devscripts"}, packageVersion = (Debian.Version.parseDebianVersion ("0.8.21.3-0+seereason1~precise1" :: String))}
       ,PackageID {packageName = BinPkgName {unBinPkgName = "hostname"}, packageVersion = (Debian.Version.parseDebianVersion ("3.06ubuntu1" :: String))}
       ,PackageID {packageName = BinPkgName {unBinPkgName = "libc6-dev"}, packageVersion = (Debian.Version.parseDebianVersion ("2.15-0ubuntu10.6" :: String))}
       ,PackageID {packageName = BinPkgName {unBinPkgName = "login"}, packageVersion = (Debian.Version.parseDebianVersion ("1:4.1.4.2+svn3283-3ubuntu5.1" :: String))}
       ,PackageID {packageName = BinPkgName {unBinPkgName = "make"}, packageVersion = (Debian.Version.parseDebianVersion ("3.81-8.1ubuntu1.1" :: String))}
       ,PackageID {packageName = BinPkgName {unBinPkgName = "mount"}, packageVersion = (Debian.Version.parseDebianVersion ("2.20.1-1ubuntu3.1" :: String))}
       ,PackageID {packageName = BinPkgName {unBinPkgName = "ncurses-base"}, packageVersion = (Debian.Version.parseDebianVersion ("5.9-4" :: String))}
       ,PackageID {packageName = BinPkgName {unBinPkgName = "ncurses-bin"}, packageVersion = (Debian.Version.parseDebianVersion ("5.9-4" :: String))}
       ,PackageID {packageName = BinPkgName {unBinPkgName = "perl-base"}, packageVersion = (Debian.Version.parseDebianVersion ("5.14.2-6ubuntu2.4" :: String))}
       ,PackageID {packageName = BinPkgName {unBinPkgName = "python-minimal"}, packageVersion = (Debian.Version.parseDebianVersion ("2.7.3-0ubuntu2.2" :: String))}
       ,PackageID {packageName = BinPkgName {unBinPkgName = "sed"}, packageVersion = (Debian.Version.parseDebianVersion ("4.2.1-9" :: String))}
       ,PackageID {packageName = BinPkgName {unBinPkgName = "tar"}, packageVersion = (Debian.Version.parseDebianVersion ("1.26-4ubuntu1" :: String))}
       ,PackageID {packageName = BinPkgName {unBinPkgName = "util-linux"}, packageVersion = (Debian.Version.parseDebianVersion ("2.20.1-1ubuntu3.1" :: String))}
       ]
