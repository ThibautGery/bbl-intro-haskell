module Main
where
import Test.Hspec

reverseRecursionWithAcc :: String -> String -> String
reverseRecursionWithAcc acc []  = acc
reverseRecursionWithAcc acc (a:q) = reverseRecursionWithAcc (a : acc) q

reverseRecursionTerminate :: String -> String
reverseRecursionTerminate = reverseRecursionWithAcc ""

reverseRecursion :: String -> String
reverseRecursion [] = []
reverseRecursion (a:b) = (reverseRecursion b) ++ [a]
-- reverseRecursion l = (reverseRecursion (tail l)) ++ [head l]
-- reverseRecursion l = (reverseRecursion $ tail l) ++ [head l]

reverseFold :: String -> String
reverseFold = foldl (\ acc a -> a : acc ) ""

main = hspec $ do
    describe "hello world" $ do
        it "1 + 1" $ do
            1 + 1 `shouldBe` 2

    describe "reverse string" $ do
        it " \"\"-> \"\"" $ do
          reverseRecursion "" `shouldBe` ""
          reverseRecursionTerminate "" `shouldBe` ""
          reverseFold "" `shouldBe` ""

        it " a -> a " $ do
          reverseRecursion "a" `shouldBe` "a"
          reverseRecursionTerminate "a" `shouldBe` "a"
          reverseFold "a" `shouldBe` "a"


        it " ab -> ba " $ do
          reverseRecursion "ab" `shouldBe` "ba"
          reverseRecursionTerminate "ab" `shouldBe` "ba"
          reverseFold "ab" `shouldBe` "ba"

        it " abc -> cba " $ do
          reverseRecursion "abc" `shouldBe` "cba"
          reverseRecursionTerminate "abc" `shouldBe` "cba"
          reverseFold "abc" `shouldBe` "cba"
