module Database.UnQLite.Bindings where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Database.UnQLite.Types

foreign import ccall "unqlite.h unqlite_open"
     c_unqlite_open :: Ptr UnQLite -> CString -> CUInt -> IO CStatusCode

foreign import ccall "unqlite.h unqlite_close"
     c_unqlite_close :: UnQLite -> IO CStatusCode

foreign import ccall "unqlite.h unqlite_kv_store"
     c_unqlite_kv_store :: UnQLite -> CString -> CInt -> CString -> CULong ->  IO CStatusCode

foreign import ccall "unqlite.h unqlite_kv_append"
     c_unqlite_kv_append :: UnQLite -> CString -> CInt -> CString -> CULong ->  IO CStatusCode

foreign import ccall "unqlite.h unqlite_kv_fetch"
     c_unqlite_kv_fetch :: UnQLite -> CString -> CInt -> CString -> Ptr CSize -> IO CStatusCode

foreign import ccall "unqlite.h unqlite_kv_delete"
     c_unqlite_kv_delete :: UnQLite -> CString -> CInt -> IO CStatusCode

foreign import ccall unsafe "stdlib.h &free" c_free_ptr :: FinalizerPtr a
