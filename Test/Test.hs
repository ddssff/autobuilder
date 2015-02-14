import System.Exit
import Test.HUnit
import qualified ParamRec
import qualified Fingerprint

main :: IO ()
main = runTestTT (TestList [ParamRec.tests, Fingerprint.tests]) >>=
       \ counts' -> exitWith (if errors counts' /= 0 || failures counts' /= 0 then ExitFailure 1 else ExitSuccess)
