module Main
where
import Test.Hspec

estNombrePremier :: Int -> Bool
estNombrePremier 1 = False
estNombrePremier n = foldl (\ acc a ->  acc && (n `mod` a /= 0)) True [2..n-1]


main = hspec $ do
    describe "hello world" $ do
        it "1 + 1" $ do
            1 + 1 `shouldBe` 2

    describe "estNombrePremier" $ do
        it " nombre premier 1 should be false" $ do
          estNombrePremier 1 `shouldBe` False

        it " nombre premier 2 should be true" $ do
          estNombrePremier 2 `shouldBe` True

        it " nombre premier 3 should be true" $ do
          estNombrePremier 3 `shouldBe` True

        it " nombre premier 4 should be false" $ do
          estNombrePremier 4 `shouldBe` False
