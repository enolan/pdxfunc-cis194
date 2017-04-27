{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TupleSections #-}
module Main(main) where

import Paths_pdxfunc_cis194

import Week1
import Week2
import Week3

import Data.Char
import Data.Function
import Data.List (foldl', sortBy)
import qualified Data.Map as M
import qualified Data.Set as S
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

main :: IO ()
main = defaultMain $ testGroup "tests" [week1Tests, week2Tests, week3Tests]

week1Tests :: TestTree
week1Tests = testGroup "Week 1" [luhns, hanoiTests]

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
  [testProperty "3 pegs (QuickCheck)" $ \(Positive n) src tgt tmp ->
      let pegs = [src, tgt, tmp] in
      n < 7 && uniqueList pegs ==>
      validateHanoi n pegs $ hanoi n src tgt tmp,
   testProperty "4 pegs (QuickCheck)" $ \(Positive n) src tgt tmp1 tmp2 ->
      let pegs = [src, tgt, tmp1, tmp2] in
      n < 7 && uniqueList pegs ==>
      validateHanoi n pegs $ hanoi2 n src tgt tmp1 tmp2
  ]

validateHanoi :: Integer -> [Peg] -> [Move] -> Bool
validateHanoi _ [] _ = error "validateHanoi called with zero pegs"
validateHanoi _ [_] _ = error "validateHanoi called with only one peg"
validateHanoi 0 _ moves = moves == []
validateHanoi n (srcPeg : tgtPeg : otherPegs) moves =
  go moves $ M.fromList $ (srcPeg, [0 .. n - 1]) : map (, []) (tgtPeg : otherPegs)
  where
  go :: [Move] -> M.Map Peg [Integer] -> Bool
  go [] pegState = pegState == M.fromList
    ([(srcPeg, []), (tgtPeg, [0 .. n - 1])] ++ map (, []) otherPegs)
  go ((moveSrc, moveDest) : movesRest) pegState =
    case (M.lookup moveSrc pegState, M.lookup moveDest pegState) of
      (Just (srcTop : srcRest), Just destDisks) ->
        if smallerThanTop srcTop destDisks
        then let newState = M.insert moveDest (srcTop : destDisks) $
                   M.insert moveSrc srcRest pegState in
          go movesRest newState
        else False
      _                               -> False

uniqueList :: (Eq a, Ord a) => [a] -> Bool
uniqueList xs = go xs S.empty
  where
  go []       _   = True
  go (y : ys) set = not (S.member y set) && go ys (S.insert y set)

smallerThanTop :: Integer -> [Integer] -> Bool
smallerThanTop _ []        = True
smallerThanTop n (x : _xs) = n < x

-- Here be spoilers!
week2Tests :: TestTree
week2Tests = testGroup "Week 2"
  [testGroup "QuickCheck"
   [testParseMessage,
    testParse,
    testInsert,
    testBuild,
    testInOrder,
    testWhatWentWrong],
   testGroup "HUnit"
   [testSampleDotLog]]

instance Arbitrary MessageType where
  arbitrary = oneof [return Info, return Warning, Error <$> arbitrary]

instance Arbitrary LogMessage where
  arbitrary = frequency
    [(5, LogMessage <$>
     oneof [return Info, return Warning, Error <$> arbitrary] <*>
     arbitrary <*> (arbitrary `suchThat` okayString)),
     (1, Unknown <$> (arbitrary `suchThat` okayString))]

-- This removes a lot of edge case tests. A real parser would have to handle
-- them, but for the purposes of the exercise let's not worry about them.
okayString :: String -> Bool
okayString [] = True
okayString str@(c : _) =
  all (\ch -> ch /= '\n' && isPrint ch && (ch == ' ' || not (isSpace ch))) str &&
  noSpaceRuns str &&
  not (isSpace c) &&
  not (isSpace $ last str)
  where
  noSpaceRuns [] = True
  noSpaceRuns [_] = True
  noSpaceRuns (c1 : rest@(c2 : _)) = not (isSpace c1 && isSpace c2) &&
    noSpaceRuns rest

instance Arbitrary MessageTree where
  arbitrary = myBuild <$> listOf arbitrary

myBuild :: [LogMessage] -> MessageTree
myBuild = foldl' myInsert Leaf

myInsert :: MessageTree -> LogMessage -> MessageTree
myInsert Leaf (Unknown _)             = Leaf
myInsert Leaf msg@(LogMessage _ _ _) = Node Leaf msg Leaf
myInsert (Node lTree msg1@(LogMessage _ ts1 _) rTree) msg2@(LogMessage _ ts2 _) =
  if ts1 <= ts2
  then Node lTree msg1 (myInsert rTree msg2)
  else Node (myInsert lTree msg2) msg1 rTree
myInsert (Node _ (Unknown _) _) _ = error "myInsert: tree with Unknown in it"
myInsert tree@(Node _ _ _) (Unknown _) = tree

myInOrder :: MessageTree -> [LogMessage]
myInOrder Leaf = []
myInOrder (Node lTree msg rTree) = myInOrder lTree ++ [msg] ++ myInOrder rTree

formatMessageType :: MessageType -> String
formatMessageType Info = "I"
formatMessageType Warning = "W"
formatMessageType (Error severity) = "E " ++ show severity

formatMessage :: LogMessage -> String
formatMessage (LogMessage ty tm msg) =
  formatMessageType ty ++ " " ++ show tm ++ " " ++ msg
formatMessage (Unknown msg) = msg

testParseMessage :: TestTree
testParseMessage =
  testProperty "parseMessage" $ \msg -> parseMessage (formatMessage msg) == msg

testParse :: TestTree
testParse = testProperty "parse" $
  \msgs -> parse (unlines $ map formatMessage msgs) == msgs

testInsert :: TestTree
testInsert = testProperty "insert" $ \tree newMsg ->
  let newTree = insert newMsg tree in
  (myInOrder newTree) == myInOrder (myInsert tree newMsg) &&
  validTree newTree

validTree :: MessageTree -> Bool
validTree Leaf = True
validTree (Node _ (Unknown _) _) = False
validTree (Node lTree (LogMessage _ ts _) rTree) =
  validLTree lTree ts && validRTree rTree ts

validLTree :: MessageTree -> TimeStamp -> Bool
validLTree Leaf _ = True
validLTree (Node _ (Unknown _) _) _ = False
validLTree (Node lTree (LogMessage _ ts _) rTree) tsGt =
  ts <= tsGt && validLTree lTree ts && validRTree rTree ts

validRTree :: MessageTree -> TimeStamp -> Bool
validRTree Leaf _ = True
validRTree (Node _ (Unknown _) _) _ = False
validRTree (Node lTree (LogMessage _ ts _) rTree) tsLt =
  ts >= tsLt && validLTree lTree ts && validRTree rTree ts

testBuild :: TestTree
testBuild = testProperty "build" $
  \msgs -> myInOrder (myBuild msgs) == myInOrder (build msgs)

testInOrder :: TestTree
testInOrder = testProperty "inOrder" $
  \tree -> myInOrder tree == inOrder tree

testWhatWentWrong :: TestTree
testWhatWentWrong = testProperty "whatWentWrong" $
  \msgs -> whatWentWrong msgs == myWhatWentWrong msgs

myWhatWentWrong :: [LogMessage] -> [String]
myWhatWentWrong msgs =
  let relevant (LogMessage (Error n) _ _) = n >= 50
      relevant _ = False
      relevantMsgs = filter relevant msgs
      getMsgStr (LogMessage _ _ str) = str
      getMsgStr (Unknown _) = error "getMsgStr Unknown" in
    map getMsgStr $ sortBy (compare `on` (\(LogMessage _ ts _) -> ts)) relevantMsgs

testSampleDotLog :: TestTree
testSampleDotLog = testCase "sample.log (from the course website)" $ do
  sampleDotLog <- getDataFileName "sample.log" >>= readFile
  assertEqual "" sampleDotLogCorrect (parse sampleDotLog)

sampleDotLogCorrect :: [LogMessage]
sampleDotLogCorrect =
  [LogMessage Info 6 "Completed armadillo processing",
   LogMessage Info 1 "Nothing to report",
   LogMessage Info 4 "Everything normal",
   LogMessage Info 11 "Initiating self-destruct sequence",
   LogMessage (Error 70) 3 "Way too many pickles",
   LogMessage (Error 65) 8 "Bad pickle-flange interaction detected",
   LogMessage Warning 5 "Flange is due for a check-up",
   LogMessage Info 7 "Out for lunch, back in two time steps",
   LogMessage (Error 20) 2 "Too many pickles",
   LogMessage Info 9 "Back from lunch",
   LogMessage (Error 99) 10 "Flange failed!"]

week3Tests :: TestTree
week3Tests = testGroup "Week 3" [skipsTests, localMaximaTests, histogramTests]

skipsTests :: TestTree
skipsTests = testGroup "skips"
  [mkTestStr skips "ABCD" ["ABCD", "BD", "C", "D"],
   mkTestStr skips "hello!" ["hello!", "el!", "l!", "l", "o", "!"],
   mkTest skips ([1] :: [Int]) [[1]],
   mkTest skips [True, False] [[True, False], [False]],
   mkTest skips ([] :: [Bool]) []]

mkTestStr :: (Show a, Eq a) => (String -> a) -> String -> a -> TestTree
mkTestStr f input expected = testCase input $
  assertEqual eqTestDesc (f input) expected

mkTest :: (Show a, Show t, Eq a) => (t -> a) -> t -> a -> TestTree
mkTest f input expected = testCase (show input) $
  assertEqual eqTestDesc (f input) expected

eqTestDesc :: [Char]
eqTestDesc = "output should equal expected from homework PDF"

localMaximaTests :: TestTree
localMaximaTests = testGroup "localMaxima"
  [mkTest localMaxima [2,9,5,6,1] [9, 6],
   mkTest localMaxima [2,3,4,1,5] [4],
   mkTest localMaxima [1,2,3,4,5] []]

histogramTests :: TestTree
histogramTests = testGroup "histogram"
  [] -- Fix this later
