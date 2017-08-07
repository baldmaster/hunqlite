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

foreign import ccall "unqlite.h unqlite_begin"
     c_unqlite_begin :: UnQLite -> IO CStatusCode

foreign import ccall "unqlite.h unqlite_commit"
     c_unqlite_commit :: UnQLite -> IO CStatusCode

foreign import ccall "unqlite.h unqlite_rollback"
     c_unqlite_rollback :: UnQLite -> IO CStatusCode

-- Maybe there's better solution to handle variadic functions...
-- Two args config
foreign import ccall "unqlite.h unqlite_config"
     c_unqlite_config_2 :: UnQLite -> CConfigOption -> Ptr CString -> Ptr CInt -> IO CStatusCode

-- One arg config
foreign import ccall "unqlite.h unqlite_config"
     c_unqlite_config_1 :: UnQLite -> CConfigOption -> Ptr CString -> IO CStatusCode

-- No arg config
foreign import ccall "unqlite.h unqlite_config"
     c_unqlite_config_0 :: UnQLite -> CConfigOption -> IO CStatusCode

-- | <https://unqlite.org/c_api/unqlite_compile.html>
foreign import ccall "unqlite.h unqlite_compile"
     c_unqlite_compile  :: UnQLite -> Jx9Script -> CInt -> Ptr VMp -> IO CStatusCode

-- | <https://unqlite.org/c_api/unqlite_value_alloc.html>
foreign import ccall "unqlite.h unqlite_vm_new_scalar"
     c_unqlite_vm_new_scalar :: VMp -> IO UnQLiteValue

foreign import ccall "unqlite.h unqlite_vm_new_array"
     c_unqlite_vm_new_array :: VMp -> IO UnQLiteValue

foreign import ccall "unqlite.h unqlite_vm_release_value"
     c_unqlite_vm_release_value :: VMp -> UnQLiteValue -> IO CStatusCode

-- | <https://unqlite.org/c_api/unqlite_vm_config.html>
foreign import ccall "unqlite.h unqlite_vm_config"
     c_unqlite_vm_config_create_var :: VMp -> CVMConfigOption -> CString -> UnQLiteValue -> IO CStatusCode

foreign import ccall "unqlite.h unqlite_vm_config"
     c_unqlite_vm_config_extract_output :: VMp -> CVMConfigOption -> Ptr CString -> Ptr CUInt -> IO CStatusCode

-- | <https://unqlite.org/c_api/unqlite_vm_exec.html>
foreign import ccall "unqlite.h unqlite_vm_exec"
     c_unqlite_vm_exec :: VMp -> IO CStatusCode

-- | <https://unqlite.org/c_api/unqlite_vm_reset.html>
foreign import ccall "unqlite.h unqlite_vm_reset"
     c_unqlite_vm_reset :: VMp -> IO CStatusCode

-- | <https://unqlite.org/c_api/unqlite_vm_release.html>
foreign import ccall "unqlite.h unqlite_vm_release"
     c_unqlite_vm_release :: VMp -> IO CStatusCode

-- | <https://unqlite.org/c_api/unqlite_vm_extract_variable.html>
foreign import ccall "unqlite.h unqlite_vm_extract_variable"
     c_unqlite_vm_extract_variable :: VMp -> CString -> IO UnQLiteValue


foreign import ccall unsafe "stdlib.h &free" c_free_ptr :: FinalizerPtr a
