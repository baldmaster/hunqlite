{-# LANGUAGE OverloadedStrings #-}

import Database.UnQLite
import Database.UnQLite.Types
import Foreign.C.Types
import Foreign.C.String
import Test.Hspec
import Control.Exception
import Test.Hspec.Expectations
import Data.Text
import qualified Data.ByteString.Char8 as BS

dbName = pack "testdb"

openConn :: IO UnQLiteHandle
openConn = openHandle dbName createMode

closeConn :: UnQLiteHandle -> IO ()
closeConn h = do
  status <- closeHandle h
  return ()

withDatabaseConnection :: (UnQLiteHandle -> IO ()) -> IO ()
withDatabaseConnection = bracket openConn closeConn

main :: IO ()
main = hspec $ do
  describe "Connection" $ do
    it "Should be able to open and close db connection" $ do
      connection <- open dbName createMode >>= newUnQLiteHandle

      status  <- closeHandle connection
      status `shouldBe` ()

  around withDatabaseConnection $ do
    describe "Store/append/fetch/delete" $ do
      it "Should successfully store key" $ \connection -> do
        result <- kvStore connection "key" "value"
        result `shouldBe` ()
        return <- kvStore connection "newKey" "value"
        result `shouldBe` ()

      it "Should successfully append key" $ \connection -> do
        result <- kvAppend connection "newKey" "value"
        result `shouldBe` ()

      it "Should successfully fetch data" $ \connection -> do
        val <- kvFetch connection "key"
        val `shouldBe` "value"
        val <- kvFetch connection "newKey"
        val `shouldBe` "valuevalue"

      it "Should successfully delete key" $ \connection -> do
        kvDelete connection "key" `shouldReturn` ()
        kvFetch connection "key" `shouldThrow` anyException
