module Main
where
import Test.Hspec

reverseCustom :: String -> String
reverseCustom [] = []
reverseCustom (a:b) = (reverseCustom b) ++ [a]

main = hspec $ do
    describe "hello world" $ do
        it "1 + 1" $ do
            1 + 1 `shouldBe` 2

    describe "reverse string" $ do
        it " \"\"-> \"\"" $ do
          reverseCustom "" `shouldBe` ""

        it " a -> a " $ do
          reverseCustom "a" `shouldBe` "a"


        it " ab -> ba " $ do
          reverseCustom "ab" `shouldBe` "ba"

        it " abc -> cba " $ do
          reverseCustom "abc" `shouldBe` "cba"
