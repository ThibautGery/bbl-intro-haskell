module Main
where
import Test.Hspec

reverseRecursion :: String -> String
reverseRecursion [] = []
reverseRecursion (a:b) = (reverseRecursion b) ++ [a]
-- reverseRecursion l = (reverseRecursion (tail l)) ++ [head l]
-- reverseRecursion l = (reverseRecursion $ tail l) ++ [head l]

main = hspec $ do
    describe "hello world" $ do
        it "1 + 1" $ do
            1 + 1 `shouldBe` 2

    describe "reverse string" $ do
        it " \"\"-> \"\"" $ do
          reverseRecursion "" `shouldBe` ""

        it " a -> a " $ do
          reverseRecursion "a" `shouldBe` "a"


        it " ab -> ba " $ do
          reverseRecursion "ab" `shouldBe` "ba"

        it " abc -> cba " $ do
          reverseRecursion "abc" `shouldBe` "cba"
