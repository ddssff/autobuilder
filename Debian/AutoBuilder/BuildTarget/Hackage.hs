{-# LANGUAGE CPP, PackageImports, ScopedTypeVariables, TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-type-defaults -fno-warn-missing-signatures #-}
module Debian.AutoBuilder.BuildTarget.Hackage
    ( prepare
    , documentation
    ) where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as Z
import Control.Exception (SomeException, throw)
import Control.Monad (when)
import Control.Monad.Catch (catch)
import Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.List (isPrefixOf, tails, intercalate)
import Data.Maybe (fromMaybe)
import Data.Set (empty)
import Data.Version (Version, showVersion, parseVersion)
import qualified Debian.AutoBuilder.Types.CacheRec as P
import qualified Debian.AutoBuilder.Types.Download as T
import qualified Debian.AutoBuilder.Types.Packages as P
import qualified Debian.AutoBuilder.Types.ParamRec as P
import Debian.Repo
import System.Exit
import System.Directory (doesFileExist, createDirectoryIfMissing, removeFile)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import System.Process (shell, showCommandForUser)
import System.Process.Read (readProcessWithExitCode)
import System.Process.Progress (collectOutputs)
import System.Unix.Directory (removeRecursiveSafely)
--import Text.XML.HaXml (htmlprint)
--import Text.XML.HaXml.Types
--import Text.XML.HaXml.Posn
import Text.ParserCombinators.ReadP (readP_to_S)

documentation :: [String]
documentation = [ "debianize:<name> or debianize:<name>=<version> - a target of this form"
                , "(currently) retrieves source code from http://hackage.haskell.org and runs"
                , "cabal-debian to create the debianization." ]

prepare :: (MonadRepos m, MonadTop m) => P.CacheRec -> P.Packages -> String -> m T.Download
prepare cache package name =
    do (version' :: Version) <- liftIO $ maybe (getVersion (P.hackageServer (P.params cache)) name) (return . readVersion) versionString
       tar <- tarball name version'
       unp <- unpacked name version'
       liftIO $ when (P.flushSource (P.params cache)) (removeRecursiveSafely tar)
       download cache name version'
       tree <- liftIO $ (findSourceTree unp :: IO SourceTree)
       return $ T.Download { T.package = package
                           , T.getTop = topdir tree
                           , T.logText =  "Built from hackage, revision: " ++ show (P.spec package)
                           , T.mVersion = Just version'
                           , T.origTarball = Just tar
                           , T.cleanTarget = \ _ -> return ([], 0)
                           , T.buildWrapper = id
                           , T.attrs = empty }
    where
      versionString = case P.testPackageFlag (\ x -> case x of P.CabalPin s -> Just s; _ -> Nothing) package of
                        [] -> Nothing
                        [v] -> Just v
                        vs -> error ("Conflicting cabal version numbers passed to Debianize: [" ++ intercalate ", " vs ++ "]")

-- | Download and unpack the given package version to the autobuilder
-- hackage temporary directory.  After the download it tries to untar
-- the file, and then it saves the compressed tarball.
download :: (MonadRepos m, MonadTop m) => P.CacheRec -> String -> Version -> m ()
download cache name version =
    (unpacked name version) >>=
    liftIO . removeRecursiveSafely >>
    downloadCached (P.hackageServer (P.params cache)) name version >>=
    unpack

{-
-- |Download and unpack the given package version to the autobuilder's
-- hackage temporary directory:
-- > download \"/home/dsf/.autobuilder/hackage\" -> \"/home/dsf/.autobuilder/hackage/happstack-server-6.1.4.tar.gz\"
-- After the download it tries to untar the file, and then it saves the compressed tarball.
downloadAndDebianize :: P.CacheRec -> [P.PackageFlag] -> String -> Version -> IO ()
downloadAndDebianize cache flags name version =
    download  cache flags name version >>
    debianize cache flags (unpacked (P.topDir cache) name version)
-}

-- |Scan the flag list for Patch flag, and apply the patches
{-
patch :: P.CacheRec -> [P.PackageFlag] -> String -> Version -> IO ()
patch cache flags name version =
    mapM_ patch' flags
    where
      patch' :: P.PackageFlag -> IO ()
      patch' (P.Patch text) =
          do (_out, err, res) <- lazyProcessE "/usr/bin/patch" ["-p1"] (Just (unpacked cache name version)) Nothing text >>=
                                 return . collectOutputUnpacked
             case res of
               ExitFailure n -> error ("patch " ++ show (unpacked cache name version) ++ " -> " ++
                                       show n ++ "\noutput: " ++ err ++ "\npatch:\n" ++ B.unpack text)
               ExitSuccess -> return ()
      patch' _ = return ()
-}

readVersion :: String -> Version
readVersion text =
    fst .
    head' .
    filter (null . snd) .
    readP_to_S parseVersion $
    text

scrapeVersion :: String -> Version
scrapeVersion text =
    readVersion .
    trimInfix "</strong>" .
    fromMaybe (error $ "Debian.AutoBuilder.BuildTarget.Hackage.readVersion 1 " ++ show text) .
    dropInfix "<strong>" $
    text

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

-- |Download and unpack the given package version to the autobuilder's
-- hackage temporary directory.  After the download it validates the
-- tarball text and saves the compressed tarball.
downloadCached :: (MonadRepos m, MonadTop m) => String -> String -> Version -> m B.ByteString
downloadCached server name version =
    do path <- tarball name version
       exists <- liftIO $ doesFileExist path
       case exists of
         True -> (liftIO (B.readFile path) >>=
                  return . validate >>=
                  maybe (download' server name version) return)
                   `catch` (\ (e :: SomeException) ->
                                     let msg = "Failure reading " ++ path ++ ": " ++ show e in
                                     liftIO (hPutStrLn stderr msg >>
                                             hPutStrLn stderr ("Removing " ++ path) >>
                                             removeFile path) >>
                                     download' server name version)
         False -> download' server name version

-- |Given a package name, get the newest version in hackage of the hackage package with that name:
-- > getVersion \"binary\" -> \"0.5.0.2\"
getVersion :: String -> String -> IO Version
getVersion server name =
    do result@(code, out, _) <- readProcessWithExitCode cmd args B.empty
       case code of
         -- This is bad it assumes the first occurrence of <strong>
         -- encloses the newest package version number.  I should go
         -- back to the html parser method
         ExitSuccess -> return $ scrapeVersion $ {- findVersion name $ (htmlParse (showCommandForUser cmd args) -} (B.unpack out)
         _ -> error ("Could not get version for " ++ name ++ "\n " ++ showCommandForUser cmd args ++ " -> " ++ show result)
    where
      cmd = "curl"
      args = ["-s", url]
      url = packageURL server name

-- |Unpack and save the files of a tarball.
unpack :: (MonadRepos m, MonadTop m) => B.ByteString -> m ()
unpack text = tmpDir >>= \ tmp -> liftIO $ Tar.unpack tmp (Tar.read (Z.decompress text))

-- |Validate the text of a tarball file.
validate :: B.ByteString -> Maybe B.ByteString
validate text =
    let entries = Tar.read (Z.decompress text) in
    case Tar.foldEntries (\ _ -> either throw (Right . (+ 1))) (Right 0) Left entries of
      Left _ -> Nothing
      Right _ -> Just text

#if 0
-- | Traverse the a hackage package page to find a version number.
-- (Obsolete, since hackage2 we are using scrapeVersion, though it is
-- fragile.)
findVersion :: String -> Document Posn -> String
findVersion package (Document _ _ (Elem _name _attrs content) _) =
    case doContentList content of
      [s] -> s
      _ss -> error ("Could not find version number of " ++ package ++ " in " ++ show (map (htmlprint . (: [])) content))
    where
      doContentList [CElem (Elem (N "head") _ _) _, CElem (Elem (N "body") _ content) _] = doContentList content
      doContentList [CElem (Elem (N "div") _ _) _, CElem (Elem (N "div") _ content) _, CElem (Elem (N "div") _ _) _] = doContentList content
      doContentList (CElem (Elem (N "h1") _ _) _ : etc) = doContentList (drop (length etc - 2) etc)
      doContentList [CElem (Elem (N "h2") _ _) _, CElem (Elem (N "ul") _ content) _] = doContentList content
      doContentList (CElem (Elem (N "li") _ content) _ :  _) = doContentList content
      doContentList (CElem (Elem (N "a") _ content) _ : _) = doContentList content
      doContentList [CString _ c _] = [parseTarballName c]
      doContentList xs = error (show (map ((: []) . htmlprint . (: [])) xs))
      parseTarballName s =
          let prefix = package ++ "-"
              suffix = ".tar.gz" in
          if isPrefixOf prefix s && isSuffixOf suffix s
          then let s' = drop (length prefix) s in
               take (length s' - length suffix) s'
          else error $ "findVersion - not a tarball: " ++ show s
#endif

-- |Download and save the tarball, return its contents.
download' :: (MonadRepos m, MonadTop m) => String -> String -> Version -> m B.ByteString
download' server name version =
    do (res, out, err, _) <- liftIO (runProc (shell (downloadCommand server name version))) >>= return . collectOutputs
       tmp <- tmpDir
       tar <- tarball name version
       -- (res, out, err) <- runProcessWith
       case res of
         (ExitSuccess : _) ->
             do liftIO $ createDirectoryIfMissing True tmp
                liftIO $ B.writeFile tar out
                return out
         _ ->
             let msg = downloadCommand server name version ++ " ->\n" ++ show (err, res) in
             liftIO (hPutStrLn stderr msg) >>
             error msg

-- |Hackage paths
packageURL server name = "http://" ++ server ++ "/package/" ++ name

versionURL server name version = "http://" ++ server ++ "/package/" ++ name ++ "-" ++ showVersion version ++ "/" ++ name ++ "-" ++ showVersion version ++ ".tar.gz"

downloadCommand :: String -> String -> Version -> String
downloadCommand server name version = "curl -s '" ++ versionURL server name version ++ "'" {- ++ " > '" ++ destPath top name version ++ "'" -}

unpacked :: MonadTop m => String -> Version -> m FilePath
unpacked name version = tmpDir >>= \ tmp -> return $ tmp </> name ++ "-" ++ showVersion version

tarball :: MonadTop m => String -> Version -> m FilePath
tarball name version  = tmpDir >>= \ tmp -> return $ tmp </> name ++ "-" ++ showVersion version ++ ".tar.gz"

tmpDir :: MonadTop m => m FilePath
tmpDir = sub "hackage"
