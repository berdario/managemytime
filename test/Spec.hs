import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
--main = putStrLn "Test suite1 not yet implemented"
main = defaultMain $ testGroup "Tests" [testCase "dummy" (True @?= True)]
