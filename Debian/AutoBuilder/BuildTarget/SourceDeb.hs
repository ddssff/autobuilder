-- |A SourceDeb target modifies another target to provide an unpacked debian source tree
-- when a debian source package is found.  A debian source package is a @.dsc@ file, a
-- @.tar.gz@ file, and an optional @.diff.gz@ file.
module Debian.AutoBuilder.BuildTarget.SourceDeb where

import Control.Monad.Trans
--import qualified Data.ByteString.Lazy.Char8 as L
import Data.List
import Data.Set (empty)
import qualified Debian.AutoBuilder.Types.Download as T
import qualified Debian.AutoBuilder.Types.CacheRec as P
import qualified Debian.AutoBuilder.Types.Packages as P
import qualified Debian.Control.String as S
import Debian.Repo.Prelude (readProc)
import Debian.Repo.Internal.Repos (MonadRepos)
import qualified Debian.Version as V
import System.Directory
import System.Exit (ExitCode(..))
import System.Process (shell)
import System.Process.Progress (keepResult)
--import System.Unix.Progress.Outputs (exitCodeOnly)

documentation = [ "sourcedeb:<target> - A target of this form unpacks the source deb"
                , "retrieved by the original target and presents an unpacked source"
                , "tree for building.  Thus, the original target should retrieve a"
                , "directory containing a .dsc file, a .tar.gz, and an optional"
                , ".diff.gz file." ]

-- |Given the BuildTarget for the base target, prepare a SourceDeb BuildTarget
-- by unpacking the source deb.
prepare :: MonadRepos m => P.CacheRec -> P.Packages -> T.Download -> m T.Download
prepare _cache package base =
    do dscFiles <- liftIO (getDirectoryContents top) >>= return . filter (isSuffixOf ".dsc")
       dscInfo <- mapM (\ name -> liftIO (readFile (top ++ "/" ++ name) >>= return . S.parseControl name)) dscFiles
       case sortBy compareVersions (zip dscFiles dscInfo) of
         [] -> return $  error ("Invalid sourcedeb base: no .dsc file in " ++ show (T.method base))
         (dscName, Right (S.Control (dscInfo : _))) : _ ->
             do out <- liftIO (readProc (shell (unpack top dscName)))
                case keepResult out of
                  [ExitSuccess] -> liftIO $ makeTarget dscInfo dscName
                  _ -> error ("*** FAILURE: " ++ unpack top dscName)
         (dscName, _) : _ -> error ("Invalid .dsc file: " ++ dscName)
    where
      top = T.getTop base
      makeTarget dscInfo dscName =
          case (S.fieldValue "Source" dscInfo, maybe Nothing (Just . V.parseDebianVersion)
                     (S.fieldValue "Version" dscInfo)) of
            (Just _package, Just _version) ->
                return $ T.Download {
                             T.package = package
                           , T.getTop = top
                           , T.logText = "Source Deb: " ++ show (P.spec package)
                           , T.mVersion = Nothing
                           , T.origTarball = Nothing
                           , T.cleanTarget = \ _ -> return ([], 0)
                           , T.buildWrapper = id
                           , T.attrs = T.attrs base
                           }
            _ -> error $ "Invalid .dsc file: " ++ dscName
      unpack top dscName = "cd " ++ top ++ " && dpkg-source -x " ++ dscName
      compareVersions (name2, info2) (name1, info1) =
          case (info1, info2) of
            (Right (S.Control (para1 : _)), Right (S.Control (para2 : _))) ->
                compare (maybe Nothing (Just . V.parseDebianVersion) (S.fieldValue "Version" para1))
                        (maybe Nothing (Just . V.parseDebianVersion) (S.fieldValue "Version" para2))
            _  -> error $ "Invalid .dsc file: " ++ name1 ++ " or " ++ name2
