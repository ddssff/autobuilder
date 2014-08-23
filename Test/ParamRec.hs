module Test.ParamRec where

import Test.HUnit
import Debian.AutoBuilder.Params

tests = TestList [test1, test2, test3, test4]

test1 = TestCase $ assertEqual "Test 1 of adjustVendorTag" "+seereason" $
        adjustVendorTag "seereason"

test2 = TestCase $ assertEqual "Test 2 of adjustVendorTag" "+seereason" $
        adjustVendorTag "+seereason"

test3 = TestCase $ assertEqual "Test 3 of adjustVendorTag" "+seereason" $
        adjustVendorTag "+seereason"

test4 = TestCase $ assertEqual "Test 3 of adjustVendorTag" "++aaa" $
        adjustVendorTag "+aaa"
