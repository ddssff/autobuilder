{-# LANGUAGE CPP, FlexibleContexts, GADTs, OverloadedStrings, PackageImports, ScopedTypeVariables, TypeFamilies #-}
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
import Data.ListLike as LL (drop, length)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Monoid ((<>))
import Data.Set as Set (fromList, toList)
import Data.Text as T (breakOn, empty, null, pack, Text, unpack)
#if MIN_VERSION_Cabal(2,0,0)
import Distribution.Version (Version, mkVersion', showVersion)
import Data.Version (parseVersion)
#else
import Data.Version (Version, showVersion, parseVersion)
#endif
import Debian.AutoBuilder.Prelude (replaceFile)
import qualified Debian.AutoBuilder.Types.CacheRec as P
import qualified Debian.AutoBuilder.Types.Download as T
import qualified Debian.AutoBuilder.Types.Packages as P
import qualified Debian.AutoBuilder.Types.ParamRec as P
import Debian.Repo as DP (findSourceTree, MonadRepos, MonadTop, SourceTree, sub, topdir)
import Debian.Repo.Fingerprint (RetrieveMethod)
import GHC.IO.Exception (IOErrorType(OtherError))
import Prelude hiding (drop, length)
import System.Exit
import System.FilePath ((</>), (<.>))
import System.IO (hPutStrLn, hPutStr, stderr)
import System.IO.Error (mkIOError)
import System.Process (CreateProcess, proc, showCommandForUser, cmdspec)
import System.Process.ListLike (readCreateProcessWithExitCode, readProcessWithExitCode, showCmdSpecForUser)
import System.Unix.Directory (removeRecursiveSafely)
import Text.XML.HaXml.Html.Parse (htmlParse')
import Text.XML.HaXml.Posn (Posn(..))
import Text.XML.HaXml (Document(..), Element(..), Content(..), QName(..), AttValue(..))
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
       version <- maybe (liftIO $ getVersion' server name) return (maybe Nothing readVersion versionString)
       -- version <- liftIO $ maybe (getVersion' server name) (return . readVersion) versionString
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
      versionString :: Maybe String
      versionString =
          case Set.toList (Set.fromList (mapMaybe P.cabalPin flags)) of
            [] -> Nothing
            [v] -> Just v
            vs -> error ("Conflicting cabal version numbers passed to Debianize: [" ++ intercalate ", " vs ++ "]")

-- | Remove everything until after the first occurrence of i
dropInfix :: String -> String -> Maybe String
dropInfix i s =
    case dropWhile (not . isPrefixOf i) (tails s) of
      [] -> Nothing
      (x : _) -> Just (drop (length i) x)

-- @@
-- (Right page) <- packagePage "hackage.haskell.org" "th-desugar"
-- dropScripts page
-- @@
dropScripts :: String -> String
dropScripts s =
    case length (takeWhile (not . isPrefixOf stag) (tails s)) of
      n | n >= length s -> s
      n -> let (before, s') = splitAt n s
               s'' = drop (length stag) s'
               charsAfter = length (takeWhile (not . isPrefixOf etag) (tails s''))
               after = drop (charsAfter + length etag) s'' in
           before ++ dropScripts after
    where stag = "<script>\n"
          etag = "</script>\n"

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
        (tarRes, tarOut, _tarErr) <- liftIO (readCreateProcessWithExitCode downloadCommand L.empty)
        (cabalRes, cabalOut, _cabalErr) <- liftIO (readCreateProcessWithExitCode cabalCommand L.empty)
        liftIO $ print ("cabal file", cabalOut)
        case (tarRes, cabalRes) of
          (ExitSuccess, ExitSuccess) ->
              do liftIO $ L.writeFile tar tarOut
                 untar tarOut
                 liftIO $ replaceFile (dir </> name <.> "cabal") cabalOut
                 return $ Right dir
          (r1, r2) ->
              do let msg = "Debian.AutoBuilder.BuildTarget.Hackage.download: " ++ showCmdSpecForUser (cmdspec downloadCommand) ++ " -> " ++ show (r1, r2)
                 liftIO $ hPutStrLn stderr msg
                 return $ Left (mkIOError OtherError msg Nothing Nothing)

      downloadCommand :: CreateProcess
      downloadCommand = proc "curl" ["-s", versionURL server name version]

      cabalCommand :: CreateProcess
      cabalCommand = proc "curl" ["-s", cabalURL server name version]

      validate :: L.ByteString -> Int
      validate text = Tar.foldEntries (\ _ n -> n + 1) 0 throw (Tar.read (Z.decompress text))

      -- Unpack and save the files of a tarball.
      untar :: (MonadIO m, MonadTop m) => L.ByteString -> m ()
      untar tarText = do
        tmp <- tmpDir
        liftIO $ Tar.unpack tmp (Tar.read (Z.decompress tarText))

#if 0
tryNTimes :: Int -> IO a -> IO (Either String a) -> IO a
tryNTimes n failed action =
    hPutStr stderr " ." >> action >>= nTimes n
    where
      nTimes 0 _ = failed
      nTimes n r = either (\ _ -> hPutStr stderr "." >> action) (return . Right) r >>= nTimes (n - 1)
#endif

versionURL server name version = "http://" ++ server ++ "/package/" ++ name ++ "-" ++ showVersion version ++ "/" ++ name ++ "-" ++ showVersion version ++ ".tar.gz"
cabalURL server name version = "http://" ++ server ++ "/package/" ++ name ++ "-" ++ showVersion version ++ "/" ++ name ++ ".cabal"

unpacked :: MonadTop m => String -> Version -> m FilePath
unpacked name version = tmpDir >>= \ tmp -> return $ tmp </> name ++ "-" ++ showVersion version

tarball :: MonadTop m => String -> Version -> m FilePath
tarball name version  = tmpDir >>= \ tmp -> return $ tmp </> name ++ "-" ++ showVersion version ++ ".tar.gz"

tmpDir :: MonadTop m => m FilePath
tmpDir = sub "hackage"

---------------- version code --------------------

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
-- λ> (Right page) <- packagePage "hackage.haskell.org" "th-desugar"
-- λ> let (Right doc) = packageHtml page
getVersion :: String -> String -> IO (Either String Version)
getVersion server name = do
  page <- fmap dropScripts <$> packagePage server name
  return (page >>= packageHtml >>= scrapeVersion)

packagePage :: String -> String -> IO (Either String String)
packagePage server name =
    do result@(code, out, _) <- readProcessWithExitCode cmd args T.empty
       case code of
         ExitSuccess -> return $ Right $ stripLines ("<script>", "</script>") $ T.unpack $ fixHrefs out
         _ -> return $ Left "Failure retrieving hackage page"
    where
      cmd = "curl"
      args = ["-s", url]
      url = packageURL server name

packageHtml :: String -> Either String (Document Posn)
packageHtml = htmlParse' ""

-- | There's a parse failure between <script> and </script>, just strip that out
stripLines :: (String, String) -> String -> String
stripLines (begin, end) s = unlines (go (lines s))
    where
      go :: [String] -> [String]
      go a = case span (/= begin)  a of
               (b, []) -> b
               (b, _ : c) ->
                   b ++ case span (/= end) c of
                          (d, []) -> []
                          (d, _ : e) -> go e

scrapeVersion :: Show a => Document a -> Either String Version
scrapeVersion (Document _ _ (Elem _ _ (_ : _ : _ : CElem (Elem _ _ body) _ : _)) _) =
    doContent (findId "content" body)
    where
      doContent (Just (CElem (Elem _ _ content) _)) = doProperties (findId "properties" content)
      doContent x = error ("scrapeVersion - unexpected content: " ++ show x)
      doProperties (Just (CElem (Elem _ _ properties) _)) = doProperties' (findClass "properties" properties)
      doProperties Nothing = Left "No such package"
      doProperties' (Just (CElem (Elem _ _ properties') _)) = doProperties'' (findElt "tbody" properties')
      doProperties'' (Just (CElem (Elem _ _ properties'') _)) = doProperties''' ((filter isCElem properties'') !! 0) -- has label "Versions"
      doProperties''' (CElem (Elem _ _ properties''') _) = doVersions (findElt "td" properties''')
      doVersions (Just (CElem (Elem _ _ versions) _)) = doVersions' (findElt "strong" (filter isCElem versions))
      doVersions' (Just (CElem (Elem _ _ [CString _ s _]) _)) = maybe (Left ("Version parse failure: " ++ show s)) Right (readVersion s)

findElt tag (elt@(CElem (Elem (N tag') _ _) _) : more) | tag == tag' = Just elt
findElt tag (_ : more) = findElt tag more
findElt _ [] = Nothing

findId s (elt@(CElem (Elem _ xs _) _) : more)
    | any testId xs = Just elt
    where testId (N "id", AttValue [Left s']) | s' == s = True
          testId _ = False
findId s (_ : more) = findId s more
findId _ [] = Nothing

findClass s (elt@(CElem (Elem _ xs _) _) : more)
    | any test xs = Just elt
    where test (N "class", AttValue [Left s']) | s' == s = True
          test _ = False
findClass s (_ : more) = findClass s more
findClass _ [] = Nothing


-- data QName = N Name | QN Namespace Name
-- data Element i = Elem QName [Attribute] [Content i]
-- type Attribute = (QName, AttValue)
-- data AttValue = AttValue [Either String Reference]

isCElem (CElem {}) = True
isCElem _ = False

readVersion :: String -> Maybe Version
readVersion text =
#if MIN_VERSION_Cabal(2,0,0)
    fmap mkVersion' .
#endif
    fmap fst .
    -- fromMaybe (error ("readVersion parse failure: " ++ show text)) .
    listToMaybe .
    filter (Prelude.null . snd) .
    readP_to_S parseVersion $
    text

fixHrefs :: Text -> Text
fixHrefs s =
    let badref = "href=http"
        goodref = "href=\"http"
        badclose = ">"
        goodclose = "\">" in
    case breakOn badref s of
      (before, after) | T.null after -> before
      (before, after) ->
          let afterRef = drop (length badref) after in
          let (url, afterURL) = breakOn badclose afterRef in
          before <> goodref <> url <> goodclose <> fixHrefs (drop (length badclose) afterURL)

-- |Hackage paths
packageURL server name = "http://" ++ server ++ "/package/" ++ name
