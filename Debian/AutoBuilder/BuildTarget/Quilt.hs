{-# LANGUAGE GADTs, OverloadedStrings, PackageImports, ScopedTypeVariables #-}
-- | The quilt target takes two other targets, one a base source
-- directory and another a quilt-style patch directory, and creates
-- a build target with the patches applied to the source directory.
module Debian.AutoBuilder.BuildTarget.Quilt where

import Control.Applicative ((<$>))
import Control.Applicative.Error (Failing(..))
import Control.Monad (when)
import Control.Monad.Error (catchError)
import Control.Monad.Trans
import qualified Data.ByteString.Lazy as L
import Data.Either (partitionEithers)
import Data.List (intercalate, sortBy)
import Data.Maybe
import Data.Set (union)
import Data.Time
import Data.Time.LocalTime ()
import Debian.AutoBuilder.Target (decode)
import qualified Debian.AutoBuilder.Types.Download as T
import qualified Debian.AutoBuilder.Types.Packages as P
import Debian.Changes (ChangeLogEntry(..), parseEntries, parseEntry)
import Debian.Pretty (ppShow)
import Debian.Repo (HasDebDir(debdir), HasTopDir(topdir), SourceTree, DebianBuildTree, findSourceTree, findOneDebianBuildTree, copySourceTree, sub, MonadRepos, MonadTop)
import Debian.Repo.Fingerprint (RetrieveMethod, retrieveMethodMD5)
import Debian.Repo.Prelude.Process (readProcessVE, readProcessV)
import Debian.Version
import Extra.Files (replaceFile)
import Extra.List ()
import System.Directory (doesFileExist, createDirectoryIfMissing, doesDirectoryExist, renameDirectory)
import System.Exit (ExitCode(ExitSuccess, ExitFailure))
import System.FilePath ((</>))
import System.Process (shell)
import Debian.Repo.Prelude.Verbosity (qPutStrLn)
import Text.Regex

qMessage s x = qPutStrLn s >> return x

documentation :: [String]
documentation = [ "quilt:(<target1>):(<target2>) - In a target of this form, target1 is"
                , "any source tree, and target2 is a quilt directory which contains"
                , "a debian style changelog file named 'changelog', a file named"
                , "'series' with a list of patch file names, and finally the patch"
                , "files listed in the series file.  The quilt system is used to apply"
                , "the patches to the source tree before building." ]

data EntryType = Base ChangeLogEntry | Patch ChangeLogEntry

getEntry (Base x) = x
getEntry (Patch x) = x

quiltPatchesDir = "quilt-patches"

makeQuiltTree :: (MonadRepos m, MonadTop m, T.Download a, T.Download b) => RetrieveMethod -> a -> b -> m (SourceTree, FilePath)
makeQuiltTree m base patch =
    do qPutStrLn $ "Quilt base: " ++ T.getTop base
       qPutStrLn $ "Quilt patch: " ++ T.getTop patch
       -- This will be the top directory of the quilt target
       copyDir <- sub ("quilt" </> retrieveMethodMD5 m)
       quilt <- sub "quilt"
       liftIO (createDirectoryIfMissing True quilt)
       baseTree <- liftIO $ findSourceTree (T.getTop base)
       patchTree <- liftIO $ (findSourceTree (T.getTop patch) :: IO SourceTree)
       copyTree <- liftIO $ copySourceTree baseTree copyDir
       -- If this is already a DebianBuildTree we need to apply
       -- the patch to the subdirectory containing the DebianSourceTree.
       debTree <- liftIO (findOneDebianBuildTree copyDir :: IO (Maybe DebianBuildTree))
       -- Compute the directory where the patches will be applied
       let quiltDir = maybe copyDir debdir debTree
       qPutStrLn $ "copyDir: " ++ copyDir
       qPutStrLn $ "quiltDir: " ++ quiltDir
       let patchDir = topdir patchTree
       -- Set up links to the quilt directory, and use quilt to get a
       -- list of the unapplied patches.
       let cmd1 = ("set -x && cd '" ++ quiltDir ++ "' && rm -f '" ++ quiltPatchesDir ++
                   "' && ln -s '" ++ patchDir ++ "' '" ++ quiltPatchesDir ++ "'")
       -- runTaskAndTest (linkStyle (commandTask cmd1))
       _output <- readProcessV (shell cmd1) L.empty
       -- Now we need to have created a DebianSourceTree so
       -- that there is a changelog for us to reconstruct.
       return (copyTree, quiltDir)

failing f _ (Failure x) = f x
failing _ s (Success x) = s x

data (T.Download a, T.Download b) => QuiltDL a b
    = QuiltDL
      { method :: RetrieveMethod
      , flags :: [P.PackageFlag]
      , base :: a
      , patch :: b
      , tree :: SourceTree
      }

instance (T.Download a, T.Download b) => T.Download (QuiltDL a b) where
    method = method
    flags = flags
    getTop = topdir . tree
    logText x = "Quilt revision " ++ show (method x)
    flushSource _ = error "QuiltDB flushSource unimplemented"
    cleanTarget x = (\ top -> T.cleanTarget (base x) top)
    attrs x = union (T.attrs (base x)) (T.attrs (patch x))

prepare :: (MonadRepos m, MonadTop m, T.Download a, T.Download b) => RetrieveMethod -> [P.PackageFlag] -> a -> b -> m T.SomeDownload
prepare method flags base patch = do
    qPutStrLn "Preparing quilt target"
    T.SomeDownload <$> (makeQuiltTree method base patch >>= liftIO . withUpstreamQuiltHidden make)
    where
      withUpstreamQuiltHidden make (quiltTree, quiltDir) =
          hide >> make (quiltTree, quiltDir) >>= unhide
          where hide = doesDirectoryExist pc >>= (flip when) (rmrf pch >> renameDirectory pc pch)
                unhide x = doesDirectoryExist pch >>= (flip when) (rmrf pc >> renameDirectory pch pc) >> return x
                pc = (quiltDir ++ "/.pc")
                pch = (quiltDir ++ "/.pc.hide")
                rmrf d = readProcessV (shell ("rm -rf '"  ++ d ++ "'")) L.empty
      make :: (SourceTree, FilePath) -> IO T.SomeDownload
      make (quiltTree, quiltDir) =
          do applied <- readProcessVE (shell cmd1a) "" >>= qMessage "Checking for applied patches"
             case applied of
               Right (ExitFailure 1, _, err)
                   | decode err == "No patches applied\n" ->
                          findUnapplied >>= apply >> buildLog >> cleanSource
                          where
                            findUnapplied = do unapplied <- liftIO (readProcessVE (shell cmd1b) "") >>= qMessage "Checking for unapplied patches"
                                               case unapplied of
                                                 Right (ExitSuccess, text, _) -> return (lines (decode text))
                                                 _ -> fail $ target ++ " - No patches to apply"
                            apply patches =
                                do result2 <- liftIO (readProcessVE (shell (cmd2 patches)) "") >>= qMessage "Patching Quilt target"
                                   case result2 of
                                     Right (ExitSuccess, _, _) -> return ()
                                     Right (_, _, err) -> fail $ target ++ " - Failed to apply quilt patches: " ++ decode err
                                     _ -> fail $ target ++ " - Failed to apply quilt patches: " ++ show result2
                            buildLog =
                                -- If there is a changelog file in the quilt directory,
                                -- interleave its entries with those in changelog of the base
                                -- tree by date.
                                do qPutStrLn "Merging changelogs"
                                   exists <- doesFileExist (quiltDir ++ "/" ++ quiltPatchesDir ++ "/changelog")
                                   case exists of
                                     False -> fail (target ++ "- Missing changelog file: " ++ show (quiltDir ++ "/" ++ quiltPatchesDir ++ "/changelog"))
                                     True -> mergeChangelogs' (quiltDir ++ "/debian/changelog") (quiltDir ++ "/" ++ quiltPatchesDir ++ "/changelog")
                            cleanSource =
                                do result3 <- liftIO (readProcessVE (shell cmd3) L.empty) >>= qMessage "Cleaning Quilt target"
                                   case result3 of
                                     Right (ExitSuccess, _, _) ->
                                         do tree <- findSourceTree (topdir quiltTree) :: IO SourceTree
                                            -- return $ Quilt base patch tree m
                                            return $ T.SomeDownload $ QuiltDL { method = method
                                                                              , flags = flags
                                                                              , base = T.SomeDownload base
                                                                              , patch = T.SomeDownload patch
                                                                              , tree = tree }
                                     _ -> fail $ target ++ " - Failure removing quilt directory: " ++ cmd3
               Right (ExitFailure _, _, err) -> fail $ target ++ " - Unexpected output from quilt applied: " ++ decode err
               _ -> fail $ target ++ " - Unexpected result code (ExitSuccess) from " ++ show cmd1a
          where
            cmd1a = ("export QUILT_PATCHES=" ++ quiltPatchesDir ++ " && cd '" ++ quiltDir ++ "' && quilt applied")
            cmd1b = ("export QUILT_PATCHES=" ++ quiltPatchesDir ++ " && cd '" ++ quiltDir ++ "' && quilt unapplied")
            -- Apply all the unapplied patches, which should be all of
            -- the patches.  This somewhat roundabout two step process
            -- is required to make sure we get an error result if any
            -- of the patches fail.
            cmd2 patches =
                ("export QUILT_PATCHES=" ++ quiltPatchesDir ++
                 " && cd '" ++ quiltDir ++ "' && " ++
                 intercalate " && " (map ("quilt -v --leave-reject push " ++) patches))
            cmd3 = ("cd '" ++ quiltDir ++ "' && " ++
                    "rm -rf '" ++ quiltDir ++ "/.pc' '" ++ quiltDir ++ "/" ++ quiltPatchesDir ++ "'")
            target = "quilt:(" ++ show (T.method base) ++ "):(" ++ show (T.method patch) ++ ")"

mergeChangelogs' :: FilePath -> FilePath -> IO (Either String ())
mergeChangelogs' basePath patchPath =
    (do patchText <- liftIO (readFile patchPath)
        baseText <- liftIO (readFile basePath)
        -- vEPutStrBl 1 $ "Merging changelogs: " ++ baseText ++ "\npatch:\n\n" ++ patchText
        either (return . Left)
               (\ newText -> liftIO $ Right <$> (replaceFile basePath $! newText))
               (mergeChangelogs baseText patchText))
      `catchError` (\ e -> return (Left (show e)))

partitionFailing :: [Failing a] -> ([[String]], [a])
partitionFailing [] = ([], [])
partitionFailing (x : xs) =
    f x (partitionFailing xs)
    where
      f (Failure x) (failures, successes) = (x : failures, successes)
      f (Success x) (failures, successes) = (failures, x : successes)

-- Merge the entries in the patch changelog into the base changelog,
-- merging the base and patch version numbers as we go.  It is
-- important that we read the base changelog lazily since there are
-- lots of bizarre formats in the older entries that we can't parse.
mergeChangelogs :: String -> String -> Either String String
mergeChangelogs baseText patchText =
    case partitionEithers (parseEntries patchText) of
      ([], patchEntries) ->
          let patchEntries' = map Patch patchEntries in
          let oldest = zonedTimeToUTC . myParseTimeRFC822 . logDate . getEntry . head . reverse $ patchEntries' in
          let (baseEntries, baseText') = partitionChangelog oldest baseText in
          let basePackage = maybe Nothing (Just . logPackage) (listToMaybe baseEntries) in
          let patchPackage = maybe Nothing (Just . logPackage) (listToMaybe patchEntries) in
          case basePackage == patchPackage of
            True ->
                let baseEntries' = map Base baseEntries in
                let mergedEntries = third . appendVersionNumbers . sortBy compareDate $ baseEntries' ++ patchEntries' in
                Right $ (intercalate "\n" (map ppShow mergedEntries)) ++ baseText'
            False ->
                Left $ "Package name mismatch between base and patch changelogs: " ++
                       maybe "?" id basePackage ++ " /= " ++ maybe "?" id patchPackage
      (failures, _) ->
          Left $ "Error(s) in patch changelog:\n  " ++ intercalate "\n  " (concat failures)
    where
      third (_, _, c) = c
      compareDate a b = compare (zonedTimeToUTC . myParseTimeRFC822 . getDate $ a) (zonedTimeToUTC . myParseTimeRFC822 . getDate $ b)
      getDate (Base entry) = logDate entry
      getDate (Patch entry) = logDate entry
      -- The version numbers of the patch entries need to be prefixed
      -- with the previous base version.  The base version numbers
      -- need to be suffixed with the previous patch version number.
      appendVersionNumbers entries = foldl modifyVersion (Nothing, Nothing, []) entries
      modifyVersion :: (Maybe DebianVersion, Maybe DebianVersion, [ChangeLogEntry]) -> EntryType
                    -> (Maybe DebianVersion, Maybe DebianVersion, [ChangeLogEntry])
      -- A base entry before the first patch entry, do nothing
      modifyVersion (_, Nothing, modified) (Base entry) = (Just (logVersion entry), Nothing, entry : modified)
      -- A patch entry before the first base entry, an error
      modifyVersion x@(Nothing, _, _modified) (Patch _entry) =
          -- This used to be an error:
          -- error "Patch changelog entry is older than oldest base entry"
          x
      -- Prefix a patch entry with the base version
      modifyVersion (Just baseVersion, _, modified) (Patch entry) =
          (Just baseVersion, (Just . logVersion $ entry), (newEntry : modified))
          where newEntry = entry {logVersion = buildQuiltVersion baseVersion (logVersion entry)}
      -- Suffix a base entry with the patch version
      modifyVersion (_, Just patchVersion, modified) (Base entry) =
          ((Just . logVersion $ entry), Just patchVersion, (newEntry : modified))
          where newEntry = entry {logVersion = buildQuiltVersion (logVersion entry) patchVersion}
      buildQuiltVersion baseVersion patchVersion =
          case Debian.Version.revision baseVersion of
            Just _ -> parseDebianVersion (show (prettyDebianVersion baseVersion) ++ "++" ++ show (prettyDebianVersion patchVersion))
            Nothing -> parseDebianVersion (show (prettyDebianVersion baseVersion) ++ "-" ++ show (prettyDebianVersion patchVersion))

partitionChangelog :: UTCTime -> String -> ([ChangeLogEntry], String)
partitionChangelog date text =
    case parseEntry text of
      Left _msgs -> ([], text)
      Right (entry, text') ->
          if date >= (zonedTimeToUTC . myParseTimeRFC822 . logDate $ entry)
          then ([entry], text')
          else case partitionChangelog date text' of
                 (entries, text'') -> (entry : entries, text'')

-- |This function is a bit less stringent than the official one -
-- e.g., it will accept "Wed, 10 Oct 2007 06:00:57 +0000", which the
-- official function won't.
myParseTimeRFC822 s =
    case matchRegex (mkRegex "^..., (.?.) (...) (....) (..):(..):(..) (.)(..)(..)$") s of
      Just [dom, mon, year, hour, min, sec, zoneSign, zoneHours, zoneMinutes] ->
          ZonedTime (localTime dom mon year hour min sec) (timeZone zoneSign zoneHours zoneMinutes)
      _ -> error ("Invalid date string: " ++ s)
    where
      -- spaceToZero ' ' = '0'
      -- spaceToZero x = x
      localTime dom mon year hr min sec = LocalTime (localDay dom mon year) (timeOfDay hr min sec)
      localDay dom mon year = fromGregorian (read year) (monthNumber mon) (read dom)
      timeZone "+" zoneHours zoneMinutes = TimeZone (60 * read zoneHours + read zoneMinutes) False ""
      timeZone "-" zoneHours zoneMinutes = TimeZone (- (60 * read zoneHours + read zoneMinutes)) False ""
      timeZone _ _ _ = error ("Time string has invalid time zone: " ++ s)
      timeOfDay hr min sec = TimeOfDay (read hr) (read min) (fromInteger . read $ sec)
      monthNumber "Jan" = 1
      monthNumber "Feb" = 2
      monthNumber "Mar" = 3
      monthNumber "Apr" = 4
      monthNumber "May" = 5
      monthNumber "Jun" = 6
      monthNumber "Jul" = 7
      monthNumber "Aug" = 8
      monthNumber "Sep" = 9
      monthNumber "Oct" = 10
      monthNumber "Nov" = 11
      monthNumber "Dec" = 12
      monthNumber _ = error ("Invalid month in time string: " ++ s)
