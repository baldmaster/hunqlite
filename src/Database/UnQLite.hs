{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Database.UnQLite where
import Database.UnQLite.Bindings
import Database.UnQLite.Types

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.ForeignPtr
import Data.ByteString
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Text (Text)
import qualified Foreign.Concurrent as Conc

newUnQLiteHandle :: UnQLite -> IO UnQLiteHandle
newUnQLiteHandle h@(UnQLite p) = UnQLiteHandle `fmap` Conc.newForeignPtr p close
  where close = c_unqlite_close h >> return ()

-- | Open connection
open :: Text -> IO UnQLiteHandle
open dbName =
  alloca $ \ptr -> do
  status  <- useAsCString (encodeUtf8 dbName) $ \ c_dbName ->
                c_unqlite_open ptr c_dbName (fromIntegral 4)
  case decodeStatus status of
    StatusOK -> do db <- peek ptr
                   newUnQLiteHandle db
    _ -> fail (show status)

-- | Close connection
close (UnQLiteHandle h) = do
  withForeignPtr h (\p ->
             c_unqlite_close (UnQLite p))


fetchDinamically :: UnQLite -> Ptr CSize -> CString -> IO (Either String ByteString)
fetchDinamically u ptr ck = do
  status <- c_unqlite_kv_fetch u ck (-1) nullPtr ptr
  case decodeStatus status of
    StatusOK -> do
      (CSize len) <- peek ptr
      allocaBytes (fromIntegral len) $
        \vptr -> do
          status <- c_unqlite_kv_fetch u ck (-1) vptr ptr
          case decodeStatus status of
            StatusOK -> do
              return . Right =<< packCString vptr
            _ -> return . Left . show $ decodeStatus status
    _ -> return . Left . show $ decodeStatus status


storeHelper :: ForeignPtr () -> ByteString -> ByteString -> StoreType -> IO CStatusCode
storeHelper h k v m = do
  useAsCString k $
    \ck -> do
      useAsCStringLen v $
        \ (cv, len) ->
          withForeignPtr h $
            \p -> method (UnQLite p) ck (-1) cv (fromIntegral len)
            where method = case m of
                    Store -> c_unqlite_kv_store
                    Append -> c_unqlite_kv_append


-- | Write a new record into the database.
-- If the record does not exists, it is created. Otherwise, it is replaced.
kvStore :: UnQLiteHandle -> ByteString -> ByteString -> IO CStatusCode
kvStore (UnQLiteHandle h) k v = do
  storeHelper h k v Store

-- | Write a new record into the database.
-- If the record does not exists, it is created.
-- Otherwise, the new data chunk is appended to the end of the old chunk.
kvAppend :: UnQLiteHandle -> ByteString -> ByteString -> IO CStatusCode
kvAppend (UnQLiteHandle h) k v = do
  storeHelper h k v Append

-- | Fetch a record from the database.
kvFetch :: UnQLiteHandle -> ByteString -> IO (Either String ByteString)
kvFetch (UnQLiteHandle h) k = do
  useAsCString k $
    \ck ->
      withForeignPtr h $
        \p -> alloca $
              \ptr -> fetchDinamically (UnQLite p) ptr ck

-- | Remove a record from the database.
kvDelete :: UnQLiteHandle -> ByteString -> IO CStatusCode
kvDelete (UnQLiteHandle h) k = do
  useAsCString k $
    \ck ->
      withForeignPtr h $
      \p -> c_unqlite_kv_delete (UnQLite p) ck (-1)
