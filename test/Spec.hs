import UnQLite
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Testing" $ do
    it "Should create db and store key-value" $ do
      connection <- openConnection "testdb"
      result <- kvStore connection "key" "value"
      result `shouldBe` 0
      close  <- dbClose connection
      close `shouldBe` 0
