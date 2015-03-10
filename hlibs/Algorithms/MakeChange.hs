module Algorithms.MakeChange where

import Test.HUnit


-- |Count the number of ways to make change for an amount using a set of denominations.
makeChange :: [Int] -> Int -> Int
makeChange denominations amount = change denominations (repeat 0) !! (amount - 1)
  where change [] prev = prev
        change (d:ds) prev = change ds result
          where result = zipWith (+) prev (take (d-1) (repeat 0) ++ [1] ++ result)


-- |Run Unit Tests
runTests :: IO Counts
runTests = runTestTT $ TestList allTests

allTests :: [Test]
allTests = [
  testEmptyDenominations, testSingleCoin, testSeveralCoins, testOneHundred,
  testLargerValue
 ]

testEmptyDenominations :: Test
testEmptyDenominations = TestCase $ assertEqual
  "You can't make change with no coins!" 0 (makeChange [] 123)

testSingleCoin :: Test
testSingleCoin = TestCase $ assertEqual
  "With just denomination 1 there's only one way to do it." 1 (makeChange [1] 123)

testSeveralCoins :: Test
testSeveralCoins = TestCase $ assertEqual
  "Some more coins." 6 (makeChange [1,5,10,25] 15)

testOneHundred :: Test
testOneHundred = TestCase $ assertEqual
  "" 242 (makeChange [1,5,10,25] 100)

testLargerValue :: Test
testLargerValue = TestCase $ assertEqual
  "Test a largish value." 321335886 (makeChange [200, 100, 50, 20, 10, 5, 2, 1] 1000)

