import System.Exit
import Test.HUnit
import qualified Test.ParamRec

main = runTestTT (TestList (Test.ParamRec.tests)) >>= 
       \ counts -> exitWith (if errors counts /= 0 || failures counts /= 0 then ExitFailure 1 else ExitSuccess)
