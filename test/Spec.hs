import Test.HUnit (runTestTT, Test(TestList))
import qualified ExamplesTest (tests)
import qualified InputTest (tests)

main = runTestTT $ TestList 
    [ ExamplesTest.tests
    , InputTest.tests 
    ]
