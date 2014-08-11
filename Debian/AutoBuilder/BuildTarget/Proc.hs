{-# LANGUAGE ScopedTypeVariables #-}
-- |Modify a target so that \/proc is mounted while it builds.
module Debian.AutoBuilder.BuildTarget.Proc where

documentation = [ "proc:<target> - A target of this form modifies another target by ensuring"
                , "that /proc is mounted during the build.  This target should only be"
                , "used if absolutely necessary, because it reveals details of the build"
                , "machine which might be different from the machine on which the package"
                , "is ultimately installed." ]
