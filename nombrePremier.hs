module Main
where
import Test.Hspec

estNombrePremier :: Int -> Bool
estNombrePremier 1 = False
-- estNombrePremier n = foldl (\ acc a ->  acc && (n `mod` a /= 0)) True [2..n-1]
estNombrePremier n = all (\ a ->  estPasDivisible n a) [2..n-1]

estPasDivisible :: Int -> Int -> Bool
estPasDivisible a b = (a `mod` b /= 0)

recupererLesPremiersPremiers :: Int -> [Int]
recupererLesPremiersPremiers n = take n [i | i <- [2..], estNombrePremier i]



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

    describe "récupérer les n premiers nombres premiers" $ do
        it " récupérer les deux premiers premiers -> 2, 3" $ do
          recupererLesPremiersPremiers 2 `shouldBe` [2, 3]
        it " récupérer les trois premiers premiers -> 2, 3, 5" $ do
          recupererLesPremiersPremiers 3 `shouldBe` [2, 3, 5]
