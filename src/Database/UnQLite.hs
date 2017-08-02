{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Database.UnQLite where
import Database.UnQLite.Bindings
import Database.UnQLite.Types

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.ForeignPtr
import qualified Foreign.Concurrent as Conc

newUnQLiteHandle :: UnQLite -> IO UnQLiteHandle
newUnQLiteHandle h@(UnQLite p) = UnQLiteHandle `fmap` Conc.newForeignPtr p close
  where close = c_unqlite_close h >> return ()

openConnection :: String -> IO UnQLiteHandle
openConnection dbName =
  alloca $ \ptr -> do
  st  <- withCString dbName $ \ c_dbName ->
                c_unqlite_open ptr c_dbName (fromIntegral 4)
  case decodeStatus st of
    StatusOK -> do db <- peek ptr
                   newUnQLiteHandle db
    _ -> fail ("OPEN_DB: failed to open " ++ show st)

-- TODO: calback fetching
-- TODO: refactoring needed obviously
fetchDinamically :: UnQLite -> Ptr CSize -> CString -> IO (Either String String)
fetchDinamically u ptr ck = do
  status <- c_unqlite_kv_fetch u ck (-1) nullPtr ptr
  case decodeStatus status of
    StatusOK -> do
      (CSize len) <- peek ptr
      allocaBytes (fromIntegral len) $
        \vptr -> do
          status <- c_unqlite_kv_fetch u ck (-1) (castPtr vptr :: Ptr CChar) ptr
          case decodeStatus status of
            StatusOK -> do
              s <- peekCString vptr
              return $ Right s
            _ -> return $ Left . show $ decodeStatus status
    _ -> return $ Left . show $ decodeStatus status


storeHelper h k v m = do
  ck <- newCString k
  cv <- newCString v
  (lp, len) <- newCStringLen v
  status <- withForeignPtr h
    (\p -> method (UnQLite p) ck (-1) cv (fromIntegral len))
  free ck
  free cv
  free lp
  return status
    where method = case m of
            Store -> c_unqlite_kv_store
            Append -> c_unqlite_kv_append

kvStore (UnQLiteHandle h) k v = do
  storeHelper h k v Store

kvAppend (UnQLiteHandle h) k v = do
  storeHelper h k v Append

kvFetch (UnQLiteHandle h) k = do
  ck <- newCString k
  value <-  withForeignPtr h $
            \p -> alloca $
                  \ptr -> fetchDinamically (UnQLite p) ptr ck
  free ck
  return value

kvDelete (UnQLiteHandle h) k = do
  ck <- newCString k
  status <- withForeignPtr h $
    \p -> c_unqlite_kv_delete (UnQLite p) ck (-1)
  free ck
  return status

dbClose (UnQLiteHandle h) = do
  withForeignPtr h (\p ->
             c_unqlite_close (UnQLite p))
