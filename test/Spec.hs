import Test.HUnit (runTestTT, Test(TestList))
import qualified ExamplesTest (tests)
import qualified InputTest (tests)
import qualified Day09Test (tests)

main = runTestTT $ TestList 
    [ ExamplesTest.tests
    , InputTest.tests
    , Day09Test.tests
    ]
