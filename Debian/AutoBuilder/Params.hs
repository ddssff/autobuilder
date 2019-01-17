{-# LANGUAGE FlexibleInstances, PackageImports, ScopedTypeVariables #-}
module Debian.AutoBuilder.Params
    ( computeTopDir
    , buildCache
    , findSlice
    -- , localPoolDir Debian.AutoBuilder.LocalRepo.poolDir
    , baseRelease
    , isDevelopmentRelease
    , adjustVendorTag -- Export for testing
    ) where

import Control.Lens (view)
import Control.Monad.Trans (liftIO)
import Data.List (isSuffixOf)
import Data.Maybe (catMaybes, fromMaybe)
import qualified Debian.AutoBuilder.LocalRepo as Local (subDir)
import Debian.AutoBuilder.Types.CacheRec (CacheRec(..))
import Debian.AutoBuilder.Types.ParamRec (ParamRec(..))
import Debian.Release (ReleaseName(ReleaseName, relName))
-- import Debian.Repo.MonadOS (MonadOS, buildEssential)
import Debian.Repo.MonadRepos (MonadRepos)
import Debian.Repo.Slice (NamedSliceList(..), SliceList(..))
import Debian.Repo.State.Slice (repoSources, verifySourcesList)
import Debian.Repo.Top (MonadTop, sub, TopDir(TopDir), toTop)
import System.Directory (createDirectoryIfMissing, getPermissions, writable)
import System.Environment (getEnv)
import Debian.Repo.Prelude.Verbosity (qPutStrLn)
import Debian.Sources (SourceOption(..), SourceOp(..))

-- |Create a Cache object from a parameter set.
buildCache :: (MonadRepos m, MonadTop r m) => ParamRec -> m CacheRec
buildCache params' =
    do (TopDir top) <- view toTop
       qPutStrLn ("Preparing autobuilder cache in " ++ top ++ "...")
       mapM_ (\ name -> sub name >>= \ path -> liftIO (createDirectoryIfMissing True path))
                  [".", "darcs", "deb-dir", "dists", "hackage", Local.subDir, "quilt", "tmp"]
       allSlices <- mapM parseNamedSliceList (sources params')
       let uri = maybe (uploadURI params') Just (buildURI params')
       build <- maybe (return $ SliceList { slices = [] }) (repoSources Nothing [SourceOption "trusted" OpSet ["yes"]]) uri
       return $ CacheRec {params = params', allSources = allSlices, buildRepoSources = build}
    where
      parseNamedSliceList (name, lines') =
          do sources' <- verifySourcesList Nothing lines'
             return $ NamedSliceList { sliceListName = ReleaseName name, sliceList = sources' }

-- |An instance of RunClass contains all the information we need to
-- run the autobuilder.
-- class (ParamClass a, CacheClass a) => RunClass a

-- |Make a ('ParamClass', 'CacheClass') pair an instance ParamClass,
-- CacheClass, and RunClass.
-- instance (ParamClass p) => RunClass (p, Cache)

-- Compute the top directory, try to create it, and then make sure it
-- exists.  Then we can safely return it from topDir below.
computeTopDir :: ParamRec -> IO FilePath
computeTopDir params' =
    do home <- getEnv "HOME"
       let top = fromMaybe (home ++ "/.autobuilder") (topDirParam params')
       createDirectoryIfMissing True top
       canWrite <- getPermissions top >>= return . writable
       case canWrite of
         False -> error "Cache directory not writable (are you root?)"
         True -> return top

-- |Find a release by name, among all the "Sources" entries given in the configuration.
findSlice :: CacheRec -> ReleaseName -> Either String NamedSliceList
findSlice cache dist =
    case filter ((== dist) . sliceListName) (allSources cache) of
      [x] -> Right x
      [] -> Left ("Debian.AutoBuilder.Params - no sources.list found for " ++ relName dist ++ ".  Is it in Debian.Releases.baseReleaseList?  allSources cache = " ++ show (fmap (relName . sliceListName) (allSources cache)))
      xs -> Left ("Multiple sources.lists found for " ++ relName dist ++ "\n" ++ show (map (relName . sliceListName) xs))

-- | Packages uploaded to the build release will be compatible
-- with packages in this release.
baseRelease :: ParamRec -> ReleaseName
baseRelease params' =
    maybe (error $ "Unknown release suffix: " ++ rel) ReleaseName
              (dropOneSuffix (releaseSuffixes params') rel)
    where rel = (relName (buildRelease params'))

dropSuffixMaybe :: String -> String -> Maybe String
dropSuffixMaybe suffix x =
    if isSuffixOf suffix x then Just (dropSuffixUnsafe (length suffix) x) else Nothing

dropSuffixUnsafe :: Int -> [a] -> [a]
dropSuffixUnsafe n x = take (length x - n) x

dropOneSuffix :: [String] -> String -> Maybe String
dropOneSuffix suffixes s =
    case catMaybes (map (`dropSuffixMaybe` s) suffixes) of
      [s'] -> Just s'
      _ -> Nothing

-- | Signifies that the release we are building for is a development
-- (or unstable) release.  This means we the tag we add doesn't need
-- to include @~<release>@, since there are no newer releases to
-- worry about trumping.
isDevelopmentRelease :: ParamRec -> Bool
isDevelopmentRelease params' =
    elem (topReleaseName (relName (buildRelease params'))) (developmentReleaseNames params')
    where
      topReleaseName name =
          foldr dropSuff name (releaseSuffixes params')
          where dropSuff suff name' = if isSuffixOf suff name' then dropSuffixUnsafe (length suff) name' else name'

-- |Adjust the vendor tag so we don't get trumped by Debian's new +b
-- notion for binary uploads.  The version number of the uploaded
-- binary packages may have "+bNNN" appended, which would cause
-- them to trump the versions constructed by the autobuilder.  So, we
-- prepend a "+" to the vendor string if there isn't one, and if the
-- vendor string starts with the character b or something less, two
-- plus signs are prepended.
adjustVendorTag :: String -> String
adjustVendorTag s =
    newprefix ++ suffix
    where (_oldprefix, suffix) = span (== '+') s
          newprefix = if suffix < "b" then "++" else "+" 
