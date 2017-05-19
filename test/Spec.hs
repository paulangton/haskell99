import Test.HUnit
import Lib

main :: IO ()
main = do
  runTestTT testMyLast
  return ()

testMyLast = TestCase (assertEqual "myLast [1,2,3,4]" (last [1,2,3,4]) (myLast [1,2,3,4]))
