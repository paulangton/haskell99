import Test.HUnit
import Lib

main :: IO ()
main = do
  runTestTT testMyLast
  runTestTT testRandomN
  return ()

testMyLast = TestCase (assertEqual "myLast [1,2,3,4]" (last [1,2,3,4]) (myLast [1,2,3,4]))
testRandomN = TestCase (assertEqual "randomN [1,2,3,4] 2" [1,4] (randomN [1,2,3,4] 2))
