import Database.UnQLite
import Database.UnQLite.Types
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Testing" $ do
    it "Should create db and store key-value" $ do
      connection <- openConnection "testdb"
      result <- kvStore connection "key" "value"
      decodeStatus result `shouldBe` StatusOK
      close  <- dbClose connection
      decodeStatus close `shouldBe` StatusOK
