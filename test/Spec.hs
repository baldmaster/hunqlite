{-# LANGUAGE OverloadedStrings #-}

import Database.UnQLite
import Database.UnQLite.Types
import Foreign.C.Types
import Foreign.C.String
import Test.Hspec
import Control.Exception
import Test.Hspec.Expectations
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS

dbName = T.pack "testdb"

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
        result <- store connection "key" "value"
        result `shouldBe` ()
        return <- store connection "newKey" "value"
        result `shouldBe` ()

      it "Should successfully append key" $ \connection -> do
        result <- append connection "newKey" "value"
        result `shouldBe` ()

      it "Should successfully fetch data" $ \connection -> do
        val <- fetch connection "key"
        val `shouldBe` (Just "value")
        val <- fetch connection "newKey"
        val `shouldBe` (Just "valuevalue")

      it "Should successfully delete key" $ \connection -> do
        delete connection "key" `shouldReturn` ()
        val <- fetch connection "key"
        val `shouldBe` Nothing

    describe "Transactions" $ do
      it "Should be possible to start and commit transcation" $
        \connection -> do
          disableAC connection `shouldReturn` ()
          begin connection `shouldReturn` ()
          store connection "testAC" "test" `shouldReturn` ()
          append connection "testAC" "test" `shouldReturn` ()
          commit connection `shouldReturn` ()

      it "DB should contain commited value" $
        \connection -> do
          val <- fetch connection "testAC"
          val `shouldBe` (Just "testtest")
