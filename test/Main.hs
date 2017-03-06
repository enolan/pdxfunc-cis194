{-# LANGUAGE TupleSections #-}
module Main where

import Week1

import qualified Data.Map as M
import qualified Data.Set as S
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

main :: IO ()
main = defaultMain $ testGroup "tests" [luhns, hanoiTests]

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
              487382095888311,
              8818474798706813,
              1201986593695480,
              6982679871091527,
              3300261759248522,
              4911184023267466,
              2418104020069560]

hanoiTests :: TestTree
hanoiTests = testGroup "Towers of Hanoi"
  [testProperty "3 pegs (QuickCheck)" $ \n src tgt tmp ->
      n < 5 && uniqueList [src, tgt, tmp] ==>
      hanoi n src tgt tmp == []
  ]

validateHanoi :: Integer -> [Peg] -> [Move] -> Bool
validateHanoi _ []                   _     =
  error "validateHanoi called with zero pegs"
validateHanoi n (srcPeg : otherPegs) moves =
  go moves $ M.fromList $ (srcPeg, [0 .. n - 1]) : map (, []) otherPegs
  where
  go [] _pegState = True
  go ((moveSrc, moveDest) : moves) pegState = _

uniqueList :: (Eq a, Ord a) => [a] -> Bool
uniqueList xs = go xs S.empty
  where
  go []       _   = True
  go (y : ys) set = not (S.member y set) && go ys (S.insert y set)
