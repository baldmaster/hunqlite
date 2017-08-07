{-# LANGUAGE OverloadedStrings #-}

import Database.UnQLite
import Database.UnQLite.Types
import Foreign.C.Types
import Foreign.C.String
import Test.Hspec
import Control.Exception
import System.Directory
import Test.Hspec.Expectations
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS

dbName = T.pack "testdb"

openConn :: IO UnQLiteHandle
openConn = do
  openHandle dbName createMode

closeConn :: UnQLiteHandle -> IO ()
closeConn h = do
  status <- closeHandle h
  return ()

dropDB = do
  let f = T.unpack dbName
  exists <- doesFileExist f
  if exists then
    removeFile f
  else return ()

withDatabaseConnection :: (UnQLiteHandle -> IO ()) -> IO ()
withDatabaseConnection = bracket openConn closeConn

testScript =
  "db_create('users'); /* Create the collection users */\
  \ /* Store something */ \
  \ db_store('users',{ 'name' : 'dean' , 'age' : 32 });\
  \ db_store('users',{ 'name' : 'chems' , 'age' : 27 });\
  \ print 'test';"

malformedScript =
  "db_create('u"

isVM m = case m of
  (VMp _) -> True
  _       -> False

main :: IO ()
main = hspec $ beforeAll_ dropDB $ afterAll_ dropDB $ do
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

  before openConn $ after closeConn $ do
    describe "VM" $ do
      it "Should complile and execute Jx9 script" $ \connection -> do
        vm <- compile connection testScript
        vm `shouldSatisfy` isVM
        exec vm `shouldReturn` ()
        output <- extractOutput vm
        output `shouldBe` "test"
        reset vm `shouldReturn` ()
        exec vm `shouldReturn` ()

      it "Should throw on malformed script" $ \connection -> do
        res <- try $ compile connection malformedScript :: IO (Either UnQLiteError VMp)
        res `shouldSatisfy` (\k -> case k of
                                Left err -> True
                                Right _ -> False)

      it "Should throw when trying to use released VM" $ \connection -> do
        vm <- compile connection testScript
        release vm `shouldReturn` ()
        exec vm `shouldThrow` anyException
