{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.UnQLite.Internal where

import Database.UnQLite.Bindings
import Foreign
import Foreign.C.Types
import Foreign.C.String
import Database.UnQLite.Types
import Data.ByteString as B
import Data.Text (Text)
import Data.Text.Encoding
import Data.Word
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


store
  :: ForeignPtr ()
  -> ByteString
  -> ByteString
  -> IO (Either (StatusCode, Text) ())
store h k v = storeHelper h k v Store

append
  :: ForeignPtr ()
  -> ByteString
  -> ByteString
  -> IO (Either (StatusCode, Text) ())
append h k v = storeHelper h k v Append


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

fetch :: ForeignPtr () -> ByteString -> IO (Either  (StatusCode, Text) ByteString)
fetch h k = do
  useAsCString k $
    \ck ->
      withForeignPtr h $
        \p -> alloca $
              \ptr -> fetchDinamically (UnQLite p) ptr ck

delete :: ForeignPtr () -> ByteString -> IO (Either  (StatusCode, Text) ())
delete h k = do
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

-- Transactions

begin :: ForeignPtr () -> IO (Either  (StatusCode, Text) ())
begin u = do
  withForeignPtr u $
    \p -> do
      status <- c_unqlite_begin (UnQLite p)
      case toResult () status of
        Left err ->
          return $ Left (err, "Failed to start transaction")
        Right () ->
          return $ Right ()


commit :: ForeignPtr () -> IO (Either  (StatusCode, Text) ())
commit u = do
  withForeignPtr u $
    \p -> do
      status <- c_unqlite_commit (UnQLite p)
      case toResult () status of
        Left err ->
          return $ Left (err, "Failed to commit transaction")
        Right () ->
          return $ Right ()


rollback :: ForeignPtr () -> IO (Either  (StatusCode, Text) ())
rollback u =
  withForeignPtr u $
    \p -> do
      status <- c_unqlite_rollback (UnQLite p)
      case toResult () status of
        Left err ->
          return $ Left (err, "Failed to rollback transaction")
        Right () ->
          return $ Right ()

-- Config
disableAC :: ForeignPtr () -> IO (Either (StatusCode, Text) ())
disableAC u =
  withForeignPtr u $
  \p -> do
    status <- c_unqlite_config_0 (UnQLite p) disableAutoCommit
    case toResult () status of
      Left err ->
        return $ Left (err, "Failed to disable autocommit")
      Right () ->
        return $ Right ()

-- | Jx9 related functions
compile :: ForeignPtr () -> ByteString -> IO (Either (StatusCode, Text) VMp)
compile u s =
  withForeignPtr u $
  \p ->
    useAsCStringLen s $
    \(cs, len) ->
      alloca $ \ptr -> do
      status <- c_unqlite_compile (UnQLite p) cs (fromIntegral len) ptr
      case toResult () status of
        Left err ->
          return $ Left (err, "Compilation failed")
        Right () ->
          Right <$> peek ptr


exec :: VMp -> IO (Either (StatusCode, Text) ())
exec vm =  do
  status <- c_unqlite_vm_exec vm
  case toResult () status of
    Left err ->
      return $ Left (err, "Execution failed")
    _        ->
      return $ Right ()

reset :: VMp -> IO (Either (StatusCode, Text) ())
reset vm =  do
  status <- c_unqlite_vm_reset vm
  case toResult () status of
    Left err ->
      return $ Left (err, "VM reset failed")
    _        ->
      return $ Right ()


release :: VMp -> IO (Either (StatusCode, Text) ())
release vm =  do
  status <- c_unqlite_vm_release vm
  case toResult () status of
    Left err ->
      return $ Left (err, "VM release failed")
    _        ->
      return $ Right ()

extractOutput :: VMp -> IO (Either (StatusCode, Text) ByteString)
extractOutput vm = do
  alloca $
    \buff ->
      alloca $
      \s -> do
        status <- c_unqlite_vm_config_extract_output
                  vm vmExtractOutput buff s
        case toResult () status of
          Left err ->
            return $ Left (err, "Failed to extract output")
          _ -> do
            l <- peek s
            str <- peek buff
            result <- B.packCStringLen (str, fromIntegral l)
            return $ Right result

setConsumer
  :: VMp
  -> (Ptr () -> CUInt -> Ptr () -> IO CStatusCode)
  -> IO (Either (StatusCode, Text) ())
setConsumer vm consumer = do
  fn <- c_wrap consumer
  status <- c_unqlite_vm_config_config_output vm vmOutput fn nullPtr
  case toResult () status of
    Left err ->
      return $ Left (err, "Consumer setting error")
    Right _ ->
      return $ Right ()
