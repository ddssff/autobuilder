#!/usr/bin/runhaskell

import Control.Exception (SomeException)
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo(buildDir))
import System.Process (system)
import System.Exit (ExitCode(ExitSuccess))
import System.FilePath ((</>))

main = defaultMainWithHooks simpleUserHooks {
         postBuild = \ _ _ _ lbi -> runTestScript lbi
       , runTests = \ _ _ _ lbi -> runTestScript lbi
       }

runTestScript lbi =
    -- system "runhaskell Test/Test.hs" >>= \ code ->
    system (buildDir lbi </> "autobuilder-tests/autobuilder-tests") >>= \ code ->
    if code == ExitSuccess then return () else error "Test Failure"
