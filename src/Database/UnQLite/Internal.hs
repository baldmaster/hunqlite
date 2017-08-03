{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.UnQLite.Internal where

import Database.UnQLite.Bindings
import Foreign
import Foreign.C.Types
import Foreign.C.String
import Database.UnQLite.Types
import Data.ByteString
import Data.Text (Text)
import Data.Text.Encoding
import Control.Applicative  ((<$>))

type Result a = Either StatusCode a

toResult :: a -> CStatusCode -> Result a
toResult a code = case status of
  StatusOK -> Right a
  _        -> Left status
  where status = decodeStatus code

open :: Text -> CAccessMode -> IO (Either (StatusCode, Text) UnQLite)
open dbName mode =
  alloca $ \ptr -> do
  status  <- useAsCString (encodeUtf8 dbName) $ \ c_dbName ->
                c_unqlite_open ptr c_dbName (getAccessMode mode)
  db <- peek ptr
  case toResult () status of
    Left err ->
      -- TODO: get error message from db
      return $ Left (err, "Failed to open DB")
    Right () ->
      if db == (UnQLite nullPtr) then
        fail "unqlite_open unexpectedly returned NULL"
      else
        return $ Right db


close :: UnQLite -> IO (Either (StatusCode, Text) ())
close u = do
  status <- toResult () <$> c_unqlite_close u
  case status of
    Left err ->
      return $ Left (err, "Failed to close DB")
    Right () -> return $ Right ()

storeHelper
  :: ForeignPtr () -- | UnQLite handle
  -> ByteString    -- | Key
  -> ByteString    -- | Value
  -> StoreType     -- | Store or Append
  -> IO (Either (StatusCode, Text) ())
storeHelper h k v m = do
  useAsCString k $
    \ck -> do
      useAsCStringLen v $
        \ (cv, len) ->
          withForeignPtr h $
            \p -> do
              status <- method (UnQLite p) ck (-1) cv (fromIntegral len)
              case toResult () status of
                Left err ->
                  return $ Left (err, "Store operation failed")
                Right () ->
                  return $ Right ()
                where method = case m of
                        Store -> c_unqlite_kv_store
                        Append -> c_unqlite_kv_append


kvStore
  :: ForeignPtr ()
  -> ByteString
  -> ByteString
  -> IO (Either (StatusCode, Text) ())
kvStore h k v = storeHelper h k v Store

kvAppend
  :: ForeignPtr ()
  -> ByteString
  -> ByteString
  -> IO (Either (StatusCode, Text) ())
kvAppend h k v = storeHelper h k v Append


fetchDinamically
  :: UnQLite
  -> Ptr CSize
  -> CString
  -> IO (Either (StatusCode, Text) ByteString)
fetchDinamically u ptr ck = do
  status <- c_unqlite_kv_fetch u ck (-1) nullPtr ptr
  case decodeStatus status of
    StatusOK -> do
      (CSize len) <- peek ptr
      allocaBytes (fromIntegral len) $
        \vptr -> do
          status <- c_unqlite_kv_fetch u ck (-1) vptr ptr
          case toResult () status of
            Left err ->
              return . Left $ (decodeStatus status, "Failed to fetch")
            Right () ->
              return . Right =<< packCString vptr
    _ -> return . Left $ (decodeStatus status, "Failed to get value size")

kvFetch :: ForeignPtr () -> ByteString -> IO (Either  (StatusCode, Text) ByteString)
kvFetch h k = do
  useAsCString k $
    \ck ->
      withForeignPtr h $
        \p -> alloca $
              \ptr -> fetchDinamically (UnQLite p) ptr ck

kvDelete :: ForeignPtr () -> ByteString -> IO (Either  (StatusCode, Text) ())
kvDelete h k = do
  useAsCString k $
    \ck ->
      withForeignPtr h $
      \p -> do
        status <- c_unqlite_kv_delete (UnQLite p) ck (-1)
        case toResult () status of
          Left err ->
            return $ Left (err, "Delete operation failed")
          Right () ->
            return $ Right ()
