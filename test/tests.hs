{- |
   Module     : Tests
   Copyright  : Copyright (C) 2018 Johannes Hartmann
   License    : MIT
   Maintainer : Johannes Hartmann <Johannes.Hartmann.Calw@web.de>
   Stability  : provisional
   Portability: portable
   Some tests to test the application
   Written by Johannes Hartmann, ec17512@qmul.ac.uk
-}
import Test.HUnit

import Data.Selection

main :: IO Counts
main = do
  runTestTT genericMinMaxFunctionTests
  runTestTT boolMinMaxFunctionTests
  runTestTT threeMinMaxFunctionTests
  runTestTT tupleMinMaxFunctionTests
  runTestTT utilityFunctionTests


genericMinMaxFunctionTests :: Test
genericMinMaxFunctionTests = TestList
  [ TestLabel "Description" genericMinTest,
    TestLabel "Description" genericMaxTest,
    TestLabel "Description" genericMinTestBool1,
    TestLabel "Description" genericMinTestBool2,
    TestLabel "Description" genericMaxTestBool1,
    TestLabel "Description" genericMaxTestBool2,
    TestLabel "Description" genericMinParalellTest,
    TestLabel "Description" genericMaxParalellTest]

genericMinTest :: Test
genericMinTest = TestCase (assertEqual "name" 1  (selection (epsilonMin [1..10]) id))

genericMaxTest :: Test
genericMaxTest = TestCase (assertEqual "name" 10  (selection (epsilonMax [1..10]) id))

genericMinTestBool1 :: Test
genericMinTestBool1 = TestCase (assertEqual "name" False  (selection (epsilonMin [False,True,False,True]) id))

genericMinTestBool2 :: Test
genericMinTestBool2 = TestCase (assertEqual "name" True  (selection (epsilonMin [True,True]) id))

genericMaxTestBool1 :: Test
genericMaxTestBool1 = TestCase (assertEqual "name" True  (selection (epsilonMax [False,True,False,True]) id))

genericMaxTestBool2 :: Test
genericMaxTestBool2 = TestCase (assertEqual "name" False  (selection (epsilonMax [False,False]) id))

genericMinParalellTest :: Test
genericMinParalellTest = TestCase (assertEqual "name" expected (selection (epsilonMinParalell [1..10]) id))
  where
    expected :: Int
    expected = 1

genericMaxParalellTest :: Test
genericMaxParalellTest = TestCase (assertEqual "name" expected (selection (epsilonMaxParalell [1..10]) id))
  where
    expected :: Int
    expected = 10

boolMinMaxFunctionTests :: Test
boolMinMaxFunctionTests = TestList
  [ TestLabel "Discription:" boolMinTest1,
    TestLabel "Discription:" boolMinTest2,
    TestLabel "Discription:" boolMaxTest1,
    TestLabel "Discription:" boolMaxTest2]

boolMinTest1 :: Test
boolMinTest1 = TestCase (assertEqual "name" False  (selection (epsilonMinBool [True,False,True,False]) id))

boolMinTest2 :: Test
boolMinTest2 = TestCase (assertEqual "name" True  (selection (epsilonMinBool [True,True]) id))

boolMaxTest1 :: Test
boolMaxTest1 = TestCase (assertEqual "name" True  (selection (epsilonMaxBool [True,False,True,False]) id))

boolMaxTest2 :: Test
boolMaxTest2 = TestCase (assertEqual "name" False  (selection (epsilonMaxBool [False,False]) id))

threeMinMaxFunctionTests :: Test
threeMinMaxFunctionTests = TestList
  [ TestLabel "Discription:" threeMinTest1,
    TestLabel "Discription:" threeMinTest2,
    TestLabel "Discription:" threeMinTest3,
    TestLabel "Discription:" threeMaxTest1,
    TestLabel "Discription:" threeMaxTest2,
    TestLabel "Discription:" threeMaxTest3 ]

threeMinTest1 :: Test
threeMinTest1 = TestCase (assertEqual "name" (-1)  (selection (epsilonMinThree [1,1,0,-1]) id))

threeMinTest2 :: Test
threeMinTest2 = TestCase (assertEqual "name" 0  (selection (epsilonMinThree [1,1,0]) id))

threeMinTest3 :: Test
threeMinTest3 = TestCase (assertEqual "name" 1  (selection (epsilonMinThree [1,1]) id))

threeMaxTest1 :: Test
threeMaxTest1 = TestCase (assertEqual "name" 1  (selection (epsilonMaxThree [1,1,0,-1]) id))

threeMaxTest2 :: Test
threeMaxTest2 = TestCase (assertEqual "name" 0  (selection (epsilonMaxThree [0,-1]) id))

threeMaxTest3 :: Test
threeMaxTest3 = TestCase (assertEqual "name" (-1)  (selection (epsilonMaxThree [-1,-1,-1]) id))


tupleMinMaxFunctionTests :: Test
tupleMinMaxFunctionTests = TestList
  [ TestLabel "Discription:" tupleMinTest1,
    TestLabel "Discription:" tupleMinTest2,
    TestLabel "Discription:" tupleMinTest3,
    TestLabel "Discription:" tupleMinTest4,
    TestLabel "Discription:" tupleMinTest5,
    TestLabel "Discription:" tupleMaxTest1,
    TestLabel "Discription:" tupleMaxTest2,
    TestLabel "Discription:" tupleMaxTest3,
    TestLabel "Discription:" tupleMaxTest4,
    TestLabel "Discription:" tupleMaxTest5,
    TestLabel "Discription:" tupleMinParalellTest1,
    TestLabel "Discription:" tupleMinParalellTest2,
    TestLabel "Discription:" tupleMinParalellTest3,
    TestLabel "Discription:" tupleMaxParalellTest1,
    TestLabel "Discription:" tupleMaxParalellTest2,
    TestLabel "Discription:" tupleMaxParalellTest3]

tupleMinTest1 :: Test
tupleMinTest1 = TestCase (assertEqual "name" (-1,3)  (selection (epsilonMinTuple [(1,9),(1,4),(0,2),(-1,3),(-1,5)]) id))

tupleMinTest2 :: Test
tupleMinTest2 = TestCase (assertEqual "name" (0,3)  (selection (epsilonMinTuple [(1,9),(1,4),(0,2),(0,3)]) id))

tupleMinTest3 :: Test
tupleMinTest3 = TestCase (assertEqual "name" (1,9)  (selection (epsilonMinTuple [(1,9),(1,4),(1,2),(1,3)]) id))

tupleMinTest4 :: Test
tupleMinTest4 = TestCase (assertEqual "name" (1,4)  (selection (epsilonMinTuple [(5,9),(1,4),(1,2),(1,3)]) id))

tupleMinTest5 :: Test
tupleMinTest5 = TestCase (assertEqual "name" (-4,2)  (selection (epsilonMinTuple [(5,9),(1,4),(-1,2),(-1,3), (-4,3),(-4,2)]) id))

tupleMaxTest1 :: Test
tupleMaxTest1 = TestCase (assertEqual "name" (1,4)  (selection (epsilonMaxTuple [(1,9),(1,4),(0,2),(-1,3),(-1,5)]) id))

tupleMaxTest2 :: Test
tupleMaxTest2 = TestCase (assertEqual "name" (0,4)  (selection (epsilonMaxTuple [(0,4),(0,2),(-1,3),(-1,5)]) id))

tupleMaxTest3 :: Test
tupleMaxTest3 = TestCase (assertEqual "name" (-1,5)  (selection (epsilonMaxTuple [(-1,3),(-1,5)]) id))

tupleMaxTest4 :: Test
tupleMaxTest4 = TestCase (assertEqual "name" (4,4)  (selection (epsilonMaxTuple [(4,9),(4,4),(1,2),(1,9),(0,2),(-1,3),(-1,5)]) id))

tupleMaxTest5 :: Test
tupleMaxTest5 = TestCase (assertEqual "name" (-1,5)  (selection (epsilonMaxTuple [(-1,3),(-1,5),(-4,2),(-9,19)]) id))

tupleMinParalellTest1 :: Test
tupleMinParalellTest1 = TestCase (assertEqual "name" expected (selection (epsilonMinTupleParalell [(1,9),(1,4),(0,2),(-1,3),(-1,5)]) id))
  where
    expected :: (Int,Int)
    expected = (-1,3)

tupleMinParalellTest2 :: Test
tupleMinParalellTest2 = TestCase (assertEqual "name" expected (selection (epsilonMinTupleParalell [(1,9),(1,4),(0,2),(0,4)]) id))
  where
    expected :: (Int,Int)
    expected = (0,4)

tupleMinParalellTest3 :: Test
tupleMinParalellTest3 = TestCase (assertEqual "name" expected (selection (epsilonMinTupleParalell [(1,9),(1,4)]) id))
  where
    expected :: (Int,Int)
    expected = (1,9)

tupleMaxParalellTest1 :: Test
tupleMaxParalellTest1 = TestCase (assertEqual "name" expected (selection (epsilonMaxTupleParalell [(1,9),(1,4),(0,2),(-1,3),(-1,5)]) id))
  where
    expected :: (Int,Int)
    expected = (1,4)

tupleMaxParalellTest2 :: Test
tupleMaxParalellTest2 = TestCase (assertEqual "name" expected (selection (epsilonMaxTupleParalell [(0,4),(0,2),(-1,3),(-1,5)]) id))
  where
    expected :: (Int,Int)
    expected = (0,4)

tupleMaxParalellTest3 :: Test
tupleMaxParalellTest3 = TestCase (assertEqual "name" expected (selection (epsilonMaxTupleParalell [(-1,3),(-1,5)]) id))
  where
    expected :: (Int,Int)
    expected = (-1,5)

utilityFunctionTests :: Test
utilityFunctionTests = TestList
  [ TestLabel "Discription:" tupleMinTest1,
    TestLabel "Discription:" tupleMinTest2]

utilityTest1 :: Test
utilityTest1 = TestCase (assertEqual "name" True ((quantifier $ morphismJK (epsilonMin [2,3,5,7,9])) even))

utilityTest2 :: Test
utilityTest2 = TestCase (assertEqual "name" False ((quantifier $ morphismJK (epsilonMin [1,3,5,7,9])) even))
