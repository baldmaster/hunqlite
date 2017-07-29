{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module UnQLite where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.ForeignPtr
import qualified Foreign.Concurrent as Conc

newtype UnQLiteHandle = UnQLiteHandle (ForeignPtr ())

newtype UnQLite = UnQLite (Ptr ()) deriving Storable
type Status = Int

foreign import ccall "unqlite.h unqlite_open"
     c_unqlite_open :: Ptr UnQLite -> CString -> CUInt -> IO Status

foreign import ccall "unqlite.h unqlite_close"
     c_unqlite_close :: UnQLite -> IO Status

foreign import ccall "unqlite.h unqlite_kv_store"
     c_unqlite_kv_store :: UnQLite -> CString -> CInt -> CString -> CULong ->  IO Status


newUnQLiteHandle :: UnQLite -> IO UnQLiteHandle
newUnQLiteHandle h@(UnQLite p) = UnQLiteHandle `fmap` Conc.newForeignPtr p close
  where close = c_unqlite_close h >> return ()

openConnection :: String -> IO UnQLiteHandle
openConnection dbName =
  alloca $ \ptr -> do
  st  <- withCString dbName $ \ c_dbName ->
                c_unqlite_open ptr c_dbName (fromIntegral 4)
  case st of
    0 -> do db <- peek ptr
            newUnQLiteHandle db
    _ -> fail ("OPEN_DB: failed to open " ++ show st)


kvStore (UnQLiteHandle h) k v = do
  ck <- newCString k
  cv <- newCString v
  (_, len) <- newCStringLen v
  withForeignPtr h (\p ->
             c_unqlite_kv_store (UnQLite p) ck (-1) cv (fromIntegral len))

dbClose (UnQLiteHandle h) = do
  withForeignPtr h (\p ->
             c_unqlite_close (UnQLite p))
