import Database.UnQLite
import Database.UnQLite.Types
import Foreign.C.Types
import Foreign.C.String
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Testing" $ do
    it "Should create db and store key-value" $ do
      connection <- openConnection "testdb"
      result <- kvStore connection "key" "value"
      decodeStatus result `shouldBe` StatusOK
      len <- kvFetch connection "key"
      len `shouldBe` Right "value"
      close  <- dbClose connection
      decodeStatus close `shouldBe` StatusOK
