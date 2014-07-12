{-# LANGUAGE TemplateHaskell #-}
module Debian.AutoBuilder.Version (autoBuilderVersion) where

import Data.Version ( showVersion )
-- import Distribution.Simple.Utils ( findPackageDesc )
-- import Distribution.Package ( pkgVersion )
-- import Distribution.PackageDescription ( package, packageDescription )
-- import Distribution.PackageDescription.Parse ( readPackageDescription )
-- import Distribution.Verbosity ( silent )
-- import Language.Haskell.TH ( runIO )
-- import Language.Haskell.TH.Syntax ( lift )
import Paths_AutoBuilder (version)

-- Compiles in the current version of the autobuilder by looking in the .cabal file.
-- To convert this to type Version, remove ". showVersion" from below.
autoBuilderVersion :: String
autoBuilderVersion = showVersion version
{-
autoBuilderVersion = $(runIO (findPackageDesc "." >>=
                              readPackageDescription silent >>=
                              return . pkgVersion . package . packageDescription) >>=
                       lift . showVersion)
-}
