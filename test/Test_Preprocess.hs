import NRBF.Preprocess
import Test.Hspec
    
main :: IO ()
main = hspec $ do
    
    describe "pre_parsef" $ do
        it "pre_parsef 'empty string'" $ do
            length (pre_parsef "") `shouldBe` 0

        it "pre_parsef TRAIN10X.DAT" $ do
            fs <- readFile "test//data//TRAIN10X.DAT"
            length (pre_parsef fs) `shouldBe` 20

        it "pre_parsef TEST10X.DAT" $ do
            fs <- readFile "test//data//TEST10X.DAT"
            length (pre_parsef fs) `shouldBe` 20

    describe "gridwatch" $ do
        it "preprocess_gridwatch should parse input file correctly" $ do
            fs <- readFile "test//data//gridwatch.2013-2014.csv"
            length (preprocess_gridwatch fs) `shouldBe` 104727

        it "preprocess_gridwatch_hour should reduce data to each hour" $ do
            fs <- readFile "test//data//gridwatch.2013-2014.csv"
            length (preprocess_gridwatch_hour fs) `shouldBe` 8742
