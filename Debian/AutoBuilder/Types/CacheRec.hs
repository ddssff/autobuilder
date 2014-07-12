module Debian.AutoBuilder.Types.CacheRec
    ( CacheRec(..)
    ) where

import Debian.AutoBuilder.Types.ParamRec (ParamRec)
import Debian.Repo.Slice (SliceList, NamedSliceList)

data CacheRec
    = CacheRec
    { params :: ParamRec
    -- , topDir :: FilePath -- Use MonadTop
    , allSources :: [NamedSliceList]
    , buildRepoSources :: SliceList
    }
