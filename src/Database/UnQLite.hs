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
            _ -> return $ Left ("get value: failed to fetch " ++ show status)
    _ -> return $ Left ("Could not get length" ++ show status)

kvStore (UnQLiteHandle h) k v = do
  ck <- newCString k
  cv <- newCString v
  (lp, len) <- newCStringLen v
  status <- withForeignPtr h
    (\p -> c_unqlite_kv_store (UnQLite p) ck (-1) cv (fromIntegral len))
  free ck
  free cv
  free lp
  return status

kvFetch (UnQLiteHandle h) k = do
  ck <- newCString k
  value <-  withForeignPtr h $
            \p -> alloca $
                  \ptr -> fetchDinamically (UnQLite p) ptr ck
  free ck
  return value

dbClose (UnQLiteHandle h) = do
  withForeignPtr h (\p ->
             c_unqlite_close (UnQLite p))
