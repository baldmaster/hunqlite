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


kvStore (UnQLiteHandle h) k v = do
  ck <- newCString k
  cv <- newCString v
  (_, len) <- newCStringLen v
  withForeignPtr h (\p ->
             c_unqlite_kv_store (UnQLite p) ck (-1) cv (fromIntegral len))

dbClose (UnQLiteHandle h) = do
  withForeignPtr h (\p ->
             c_unqlite_close (UnQLite p))
