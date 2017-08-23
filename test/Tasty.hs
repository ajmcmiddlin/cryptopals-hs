import Test.Tasty (defaultMain, testGroup)

import Set1Test (test_Set1)

main :: IO ()
main = defaultMain $ testGroup "Cryptopals" [
         test_Set1
       ]
