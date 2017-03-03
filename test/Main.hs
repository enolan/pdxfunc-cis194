module Main where

import Week1

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ testGroup "tests" [luhns]

luhns :: TestTree
luhns = testGroup "Credit card number validation"
  [ testGroup "valid" $ map (testCCNumber True) validCCs,
    testGroup "invalid" $ map (testCCNumber False) invalidCCs
  ]

testCCNumber :: Bool -> Integer -> TestTree
testCCNumber shouldBe ccNum = testCase (show ccNum) $ assertBool msg good
  where
  good = validateCC ccNum == shouldBe
  msg = if shouldBe
    then "validateCC should've returned True but returned False"
    else "validateCC should've returned False but returned True"

validCCs :: [Integer]
validCCs = [4161432561375371,
            4149109264359442,
            4730337425648024,
            4068980297925194,
            4387683432204,
            364863961308302,
            360205910934594,
            6011108144784134,
            6011910316770969,
            6011141212296400]

invalidCCs :: [Integer]
invalidCCs = [1817288007626273,
              4123618622277941,
              7431095047062952,
              6593725291098509,
              8818474798706813,
              1201986593695480,
              6982679871091527,
              3300261759248522,
              4911184023267466,
              2418104020069560]
