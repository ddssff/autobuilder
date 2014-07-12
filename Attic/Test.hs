module Main where

import Debian.Repo
import Data.List
--import Repository
import qualified System.IO as IO
import System.Unix.Directory
import System.Directory
import Extra.TIO

top = "/tmp/testrepo"

eputStr s = IO.hPutStr IO.stderr s

main :: IO ()
main =
    do
      removeRecursiveSafely top
      createDirectoryIfMissing True top
      repo <- runTIO defStyle . run aptIOStyle $ repoTests
      releases <- runTIO defStyle . run aptIOStyle $ releaseTests repo
      runTIO defStyle . run aptIOStyle $ uploadTests releases
      return ()

repoTests :: AptIO LocalRepository
repoTests =
    do
      tio $ vPutStr 0 "------------- Repo Tests -------------"
      tio $ vPutStr 0 (" -> prepareRepository False \"" ++ top ++ "\" (Just Pool)")
      -- This fails when it tries to stat nonexistant directories
      -- prepareRepository True top (Just Pool)
      repo <- prepareLocalRepository (EnvPath (EnvRoot "") top) (Just Pool)
      tio $ vPutStr 0 (" <- " ++ show repo)
      return repo

releaseTests :: LocalRepository -> AptIO [Release]
releaseTests repo =
    do
      tio $ vPutStr 0 "------------- Release Tests -------------"
      let dist = "testrelease"
      let aliases = ["skipjack-feisty"]
      let components = map parseSection' ["main", "contrib", "non-free"]
      let archList = [Binary "i386", Binary "amd64"]
      tio $ vPutStr 0 (" -> prepareRelease repo (ReleaseName " ++ show dist ++ ") (map ReleaseName " ++ show aliases ++ ") " ++ show (map sectionName' components) ++ " " ++ show (map archName archList))
      -- prepareRelease True repo (ReleaseName dist) (map ReleaseName aliases) components archList
      release <- prepareRelease repo (parseReleaseName dist) (map parseReleaseName aliases) components archList
      tio $ vPutStr 0 (" <- " ++ show release)
      case releaseRepo release of
        LocalRepo repo' ->
            do releases <- findReleases repo'
               tio $ vPutStr 0 "All releases:"
               mapM_ (tio . vPutStr 0) (map (("  " ++) . show) releases)
               return releases
        _ -> error "Expected local release"

dir = "/home/david/.autobuilder/localpools/CNRUbuntu/"
files = ["freespire-build-specs_0.1-0r1cnr13_all.deb",
         "freespire-build-specs_0.1-0r1cnr13_amd64.changes",
         "freespire-build-specs_0.1-0r1cnr13.dsc",
         "freespire-build-specs_0.1-0r1cnr13.tar.gz"]

uploadTests :: [Release] -> AptIO ()
uploadTests [release]  =
    do
      tio $ vPutStr 0 "------------- Upload Tests -------------"
      let (LocalRepo repo) = releaseRepo release
      io $ mapM_ upload files
      tio $ vPutStr 0 " -> dryRun $ scanIncoming repo' (Component \"main\")"
      scanIncoming True Nothing repo
      tio $ vPutStr 0 " -> realRun $ scanIncoming repo (Component \"main\")"
      (_, errors) <- scanIncoming True Nothing repo
      case errors of
        [] -> tio $ vPutStr 0 " <- Ok"
        _ -> tio $ vPutStr 0 (" <- " ++ show errors)
      -- get some package info
      let (sourceIndexes :: [PackageIndexLocal]) =
              filter (\ index -> packageIndexComponent index == parseSection' "main") (sourceIndexList release)
      tio $ vPutStr 0 ("sourceIndexes: " ++ show sourceIndexes)
      (packages :: (Either String [BinaryPackageLocal])) <- tio $ getPackages (head sourceIndexes)
      tio $ vPutStr 0 (" -> getPackages " ++ show (head sourceIndexes))
      tio $ vPutStr 0 (" <- " ++ show (either show (show . map packageID) packages))
      -- delete a source package
      -- Delete a source package that has a binary with a different version number
      return ()
    where
      upload file = readFile (dir ++ file) >>= writeFile (top ++ "/incoming/" ++ file)
uploadTests releases = error ("Too many releases: " ++ show releases)
