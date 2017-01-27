{-# LANGUAGE CPP, GADTs, PackageImports, ScopedTypeVariables, TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-type-defaults -fno-warn-missing-signatures #-}
module Debian.AutoBuilder.BuildTarget.Hackage
    ( prepare
    , documentation
    ) where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as Z
import Control.Exception (SomeException, throw, evaluate)
import Control.Monad.Catch (MonadCatch, try)
import Control.Monad.Trans (MonadIO, liftIO)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import Data.List (isPrefixOf, tails, intercalate)
import Data.Maybe (mapMaybe)
import Data.Set as Set (fromList, toList)
import Data.Version (Version, showVersion, parseVersion)
import qualified Debian.AutoBuilder.Types.CacheRec as P
import qualified Debian.AutoBuilder.Types.Download as T
import qualified Debian.AutoBuilder.Types.Packages as P
import qualified Debian.AutoBuilder.Types.ParamRec as P
import Debian.Repo as DP
import Debian.Repo.Fingerprint (RetrieveMethod)
import GHC.IO.Exception (IOErrorType(OtherError))
import System.Exit
import System.FilePath ((</>))
import System.IO (hPutStrLn, hPutStr, stderr)
import System.IO.Error (mkIOError)
import System.Process (CreateProcess, proc, showCommandForUser, cmdspec)
import System.Process.ListLike (readCreateProcessWithExitCode, readProcessWithExitCode, showCmdSpecForUser)
import System.Unix.Directory (removeRecursiveSafely)
import Text.ParserCombinators.ReadP (readP_to_S)

documentation :: [String]
documentation = [ "debianize:<name> or debianize:<name>=<version> - a target of this form"
                , "(currently) retrieves source code from http://hackage.haskell.org and runs"
                , "cabal-debian to create the debianization." ]

data HackageDL
    = HackageDL
      { server :: String -- ^ Hackage server name
      , method :: RetrieveMethod
      , flags :: [P.PackageFlag]
      , tree :: SourceTree
      , version :: Version
      , tar :: FilePath
      } deriving Show

instance T.Download HackageDL where
    method = method
    flags = flags
    getTop = topdir . tree
    logText x = "Built from hackage, revision: " ++ show (method x)
    mVersion = Just . version
    origTarball = Just . tar
    flushSource _ = error "flushSource HackageDL unimplemented"

prepare :: (MonadRepos m, MonadTop m) => P.CacheRec -> RetrieveMethod -> [P.PackageFlag] -> String -> m T.SomeDownload
prepare cache method flags name =
    do let server = P.hackageServer (P.params cache) -- typically "hackage.haskell.org"
       version <- liftIO $ maybe (getVersion' server name) (return . readVersion) versionString
       tar <- tarball name version
       unp <- downloadCached server name version
       tree <- liftIO $ (findSourceTree unp :: IO SourceTree)
       return $ T.SomeDownload $ HackageDL { server = P.hackageServer (P.params cache)
                                           , method = method
                                           , flags = flags
                                           , tree = tree
                                           , version = version
                                           , tar = tar }
    where
      versionString = case Set.toList (Set.fromList (mapMaybe P.cabalPin flags)) of
                        [] -> Nothing
                        [v] -> Just v
                        vs -> error ("Conflicting cabal version numbers passed to Debianize: [" ++ intercalate ", " vs ++ "]")

readVersion :: String -> Version
readVersion text =
    fst .
    head' .
    filter (null . snd) .
    readP_to_S parseVersion $
    text

scrapeVersion :: String -> Either String Version
scrapeVersion text =
#if 1
    case dropInfix "<strong>" text of
      Nothing -> Left $ "Debian.AutoBuilder.BuildTarget.Hackage.readVersion 1 " ++ show text
      Just x -> Right $ readVersion $ trimInfix "</strong>" $ x
#else
    readVersion .
    trimInfix "</strong>" .
    fromMaybe (error $ "Debian.AutoBuilder.BuildTarget.Hackage.readVersion 1 " ++ show text) .
    dropInfix "<strong>" $
    text
#endif

head' (x : _xs) = x
head' [] = error "Debian.AutoBuilder.BuildTarget.Hackage.readVersion 2"

-- | Remove everything until after the first occurrence of i
dropInfix :: String -> String -> Maybe String
dropInfix i s =
    case dropWhile (not . isPrefixOf i) (tails s) of
      [] -> Nothing
      (x : _) -> Just (drop (length i) x)

-- | Remove everything starting from the first occurrence of i
trimInfix :: String -> String -> String
trimInfix i s = take (length (takeWhile (not . isPrefixOf i) (tails s))) s

-- | Check for file in cache and validate, on failure download and validate.
downloadCached :: forall m. (MonadCatch m, MonadIO m, MonadTop m) => String -> String -> Version -> m FilePath
downloadCached server name version = do
  -- Look for the cached version, if that fails try unpacking the tarball, if that fails try downloading.
  cache >>= maybe (download >>= either throw return) return
    where
      cache :: m (Maybe FilePath)
      cache = do
        tar <- tarball name version
        dir <- unpacked name version
        (text' :: Either SomeException L.ByteString) <- liftIO $ try $ L.readFile tar
        (count :: Either SomeException Int) <-
            case text' of
              Left e -> return $ Left e
              Right text -> liftIO $ try (evaluate $ validate text)
        case count of
          Right _ -> return (Just dir)
          Left (_ :: SomeException) -> return Nothing
      download :: m (Either IOError FilePath)
      download = do
        tar <- tarball name version
        dir <- unpacked name version
        liftIO $ removeRecursiveSafely dir
        (res, out, _err) <- liftIO (readCreateProcessWithExitCode downloadCommand L.empty)
        case res of
          ExitSuccess ->
              do liftIO $ L.writeFile tar out
                 untar out
                 return (Right dir)
          ExitFailure r ->
              do let msg = "Debian.AutoBuilder.BuildTarget.Hackage.download: " ++ showCmdSpecForUser (cmdspec downloadCommand) ++ " -> " ++ show r
                 liftIO $ hPutStrLn stderr msg
                 return $ Left (mkIOError OtherError msg Nothing Nothing)

      downloadCommand :: CreateProcess
      downloadCommand = proc "curl" ["-s", versionURL server name version]

      validate :: L.ByteString -> Int
      validate text = Tar.foldEntries (\ _ n -> n + 1) 0 throw (Tar.read (Z.decompress text))

      -- Unpack and save the files of a tarball.
      untar :: (MonadIO m, MonadTop m) => L.ByteString -> m ()
      untar text = tmpDir >>= \ tmp -> liftIO $ Tar.unpack tmp (Tar.read (Z.decompress text))

#if 0
tryNTimes :: Int -> IO a -> IO (Either String a) -> IO a
tryNTimes n failed action =
    hPutStr stderr " ." >> action >>= nTimes n
    where
      nTimes 0 _ = failed
      nTimes n r = either (\ _ -> hPutStr stderr "." >> action) (return . Right) r >>= nTimes (n - 1)
#endif

getVersion' :: String -> String -> IO Version
getVersion' server name =
#if 0
    tryNTimes 7 (error "Hackage getVersion failed") (getVersion server name)
#else
    hPutStr stderr " ." >> getVersion server name >>=
    either (\ _ -> hPutStr stderr "." >> getVersion server name) (return . Right) >>=
    either (\ _ -> hPutStr stderr "." >> getVersion server name) (return . Right) >>=
    either (\ _ -> hPutStr stderr "." >> getVersion server name) (return . Right) >>=
    either (\ _ -> hPutStr stderr "." >> getVersion server name) (return . Right) >>=
    either (\ _ -> hPutStr stderr "." >> getVersion server name) (return . Right) >>=
    either (\ s -> hPutStr stderr "Hackage getVersion failed." >> error s) return
#endif

-- |Given a package name, get the newest version in hackage of the hackage package with that name:
-- > getVersion \"binary\" -> \"0.5.0.2\"
getVersion :: String -> String -> IO (Either String Version)
getVersion server name =
    do result@(code, out, _) <- readProcessWithExitCode cmd args B.empty
       case code of
         -- This is bad it assumes the first occurrence of <strong>
         -- encloses the newest package version number.  I should go
         -- back to the html parser method
         ExitSuccess -> return $ scrapeVersion $ B.unpack out
         _ -> return $ Left $ "Could not get version for " ++ name ++ "\n " ++ showCommandForUser cmd args ++ " -> " ++ show result
    where
      cmd = "curl"
      args = ["-s", url]
      url = packageURL server name

-- |Hackage paths
packageURL server name = "http://" ++ server ++ "/package/" ++ name

versionURL server name version = "http://" ++ server ++ "/package/" ++ name ++ "-" ++ showVersion version ++ "/" ++ name ++ "-" ++ showVersion version ++ ".tar.gz"

unpacked :: MonadTop m => String -> Version -> m FilePath
unpacked name version = tmpDir >>= \ tmp -> return $ tmp </> name ++ "-" ++ showVersion version

tarball :: MonadTop m => String -> Version -> m FilePath
tarball name version  = tmpDir >>= \ tmp -> return $ tmp </> name ++ "-" ++ showVersion version ++ ".tar.gz"

tmpDir :: MonadTop m => m FilePath
tmpDir = sub "hackage"
