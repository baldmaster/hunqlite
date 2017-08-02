{-# LANGUAGE OverloadedStrings #-}

import Database.UnQLite
import Database.UnQLite.Types
import Foreign.C.Types
import Foreign.C.String
import Test.Hspec
import Control.Exception
import Data.Text
import qualified Data.ByteString.Char8 as BS

dbName = pack "testdb"

openConn :: IO UnQLiteHandle
openConn = open dbName

closeConn :: UnQLiteHandle -> IO ()
closeConn h = do
  status <- close h
  return ()

withDatabaseConnection :: (UnQLiteHandle -> IO ()) -> IO ()
withDatabaseConnection = bracket openConn closeConn

main :: IO ()
main = hspec $ do
  describe "Connection" $ do
    it "Should be able to open and close db connection" $ do
      connection <- open dbName

      status  <- close connection
      decodeStatus status `shouldBe` StatusOK

  around withDatabaseConnection $ do
    describe "Store/append/fetch/delete" $ do
      it "Should successfully store key" $ \connection -> do
        result <- kvStore connection "key" "value"
        decodeStatus result `shouldBe` StatusOK
        return <- kvStore connection "newKey" "value"
        decodeStatus result `shouldBe` StatusOK

      it "Should successfully append key" $ \connection -> do
        result <- kvAppend connection "newKey" "value"
        decodeStatus result `shouldBe` StatusOK

      it "Should successfully fetch data" $ \connection -> do
        val <- kvFetch connection "key"
        val `shouldBe` Right "value"
        val <- kvFetch connection "newKey"
        val `shouldBe` Right "valuevalue"

      it "Should successfully delete key" $ \connection -> do
        result <- kvDelete connection "key"
        decodeStatus result `shouldBe` StatusOK
        val <- kvFetch connection "key"
        val `shouldBe` Left "StatusNotFound"
