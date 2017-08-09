{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Database.UnQLite.Types where

import Foreign
import Foreign.ForeignPtr
import Foreign.C.Types
import Foreign.C.String

#include "unqlite.h"


-- | Status codes
data StatusCode = StatusOK               -- ^ Successful result
                | StatusNoMem            -- ^ Out of memory
                | StatusAbort            -- ^ Another thread have released this instance
                | StatusIOError          -- ^ IO error
                | StatusCorrupt          -- ^ Corrupt pointer
                | StatusLocked           -- ^ Forbidden Operation
                | StatusBusy             -- ^ The database file is locked
                | StatusDone             -- ^ Operation done
                | StatusPermissionError  -- ^ Permission error
                | StatusNotImplemented   -- ^ Method not implemented by the underlying Key/Value storage engine
                | StatusNotFound         -- ^ No such record
                | StatusNoop             -- ^ No such method
                | StatusInvalid          -- ^ Invalid parameter
                | StatusEOF              -- ^ End Of Input
                | StatusUnknown          -- ^ Unknown configuration option
                | StatusLimit            -- ^ Database limit reached
                | StatusExists           -- ^ Record exists
                | StatusEmpty            -- ^ Empty record
                | StatusCompilerError    -- ^ error
                | StatusVMError          -- ^ machine error
                | StatusFull             -- ^ database (unlikely)
                | StatusCantOpen         -- ^ to open the database file
                | StatusReadOnly         -- ^ only Key/Value storage engine
                | StatusLockError        -- ^ protocol error
                deriving(Eq, Show)

-- | C status code
newtype CStatusCode = CStatusCode {unCStatusCode :: CInt}
  deriving (Show, Eq)

-- | Decode status code
decodeStatus :: CStatusCode -> StatusCode
decodeStatus (CStatusCode n) = case n of
  #{const UNQLITE_OK}             -> StatusOK
  #{const UNQLITE_NOMEM}          -> StatusNoMem
  #{const UNQLITE_ABORT}          -> StatusAbort
  #{const UNQLITE_IOERR}          -> StatusIOError
  #{const UNQLITE_CORRUPT}        -> StatusCorrupt
  #{const UNQLITE_LOCKED}         -> StatusLocked
  #{const UNQLITE_BUSY}           -> StatusBusy
  #{const UNQLITE_DONE}           -> StatusDone
  #{const UNQLITE_PERM}           -> StatusPermissionError
  #{const UNQLITE_NOTIMPLEMENTED} -> StatusNotImplemented
  #{const UNQLITE_NOTFOUND}       -> StatusNotFound
  #{const UNQLITE_NOOP}           -> StatusNoop
  #{const UNQLITE_INVALID}        -> StatusInvalid
  #{const UNQLITE_EOF}            -> StatusEOF
  #{const UNQLITE_UNKNOWN}        -> StatusUnknown
  #{const UNQLITE_LIMIT}          -> StatusLimit
  #{const UNQLITE_EXISTS}         -> StatusExists
  #{const UNQLITE_EMPTY}          -> StatusEmpty
  #{const UNQLITE_COMPILE_ERR}    -> StatusCompilerError
  #{const UNQLITE_VM_ERR}         -> StatusVMError
  #{const UNQLITE_FULL}           -> StatusFull
  #{const UNQLITE_CANTOPEN}       -> StatusCantOpen
  #{const UNQLITE_READ_ONLY}      -> StatusReadOnly
  #{const UNQLITE_LOCKERR}        -> StatusLockError

encodeStatus :: StatusCode -> CStatusCode
encodeStatus s = CStatusCode $ case s of
  StatusOK               -> #const UNQLITE_OK
  StatusNoMem            -> #const UNQLITE_NOMEM
  StatusAbort            -> #const UNQLITE_ABORT
  StatusIOError          -> #const UNQLITE_IOERR
  StatusCorrupt          -> #const UNQLITE_CORRUPT
  StatusLocked           -> #const UNQLITE_LOCKED
  StatusBusy             -> #const UNQLITE_BUSY
  StatusDone             -> #const UNQLITE_DONE
  StatusPermissionError  -> #const UNQLITE_PERM
  StatusNotImplemented   -> #const UNQLITE_NOTIMPLEMENTED
  StatusNotFound         -> #const UNQLITE_NOTFOUND
  StatusNoop             -> #const UNQLITE_NOOP
  StatusInvalid          -> #const UNQLITE_INVALID
  StatusEOF              -> #const UNQLITE_EOF
  StatusUnknown          -> #const UNQLITE_UNKNOWN
  StatusLimit            -> #const UNQLITE_LIMIT
  StatusExists           -> #const UNQLITE_EXISTS
  StatusEmpty            -> #const UNQLITE_EMPTY
  StatusCompilerError    -> #const UNQLITE_COMPILE_ERR
  StatusVMError          -> #const UNQLITE_VM_ERR
  StatusFull             -> #const UNQLITE_FULL
  StatusCantOpen         -> #const UNQLITE_CANTOPEN
  StatusReadOnly         -> #const UNQLITE_READ_ONLY
  StatusLockError        -> #const UNQLITE_LOCKERR


-- | Unqlite handle
newtype UnQLiteHandle = UnQLiteHandle (ForeignPtr ())

-- | Unqlite pointer
newtype UnQLite = UnQLite (Ptr ()) deriving (Storable, Eq)

-- | Store type
data StoreType = Store | Append

-- | DB access mode
newtype CAccessMode = CAccessMode {getAccessMode :: CUInt}
  deriving (Show, Eq)

-- | Main access modes
#{enum CAccessMode, CAccessMode
 , readOnlyMode  = UNQLITE_OPEN_READONLY
 , readWriteMode = UNQLITE_OPEN_READWRITE
 , createMode    = UNQLITE_OPEN_CREATE
 , tempDBMode    = UNQLITE_OPEN_TEMP_DB
 , noMutexMode   = UNQLITE_OPEN_NOMUTEX
 , omitJournalingMode =  UNQLITE_OPEN_OMIT_JOURNALING
 , inMemoryMode  = UNQLITE_OPEN_IN_MEMORY
 , mmapMode      = UNQLITE_OPEN_MMAP
 }

-- | DB config option
newtype CConfigOption = CConfigOption {getConfigOption :: CInt}
  deriving (Show, Eq)

-- | Config options to configure database handle.
-- <https://unqlite.org/c_api/unqlite_config.html>
#{enum CConfigOption, CConfigOption
 , jx9ErrorLog       = UNQLITE_CONFIG_JX9_ERR_LOG
 , maxPageCache      = UNQLITE_CONFIG_MAX_PAGE_CACHE
 , errorLog          = UNQLITE_CONFIG_ERR_LOG
 , kvEngine          = UNQLITE_CONFIG_KV_ENGINE
 , disableAutoCommit = UNQLITE_CONFIG_DISABLE_AUTO_COMMIT
 , kvName            = UNQLITE_CONFIG_GET_KV_NAME
 }

-- | VM config option
newtype CVMConfigOption = CVMConfigOption {getVMOption :: CInt}
  deriving (Show, Eq)

-- | VM options.
-- <https://unqlite.org/c_api/unqlite_vm_config.html>
#{enum CVMConfigOption, CVMConfigOption
 , vmOutput           = UNQLITE_VM_CONFIG_OUTPUT
 , vmImportPath       = UNQLITE_VM_CONFIG_IMPORT_PATH
 , vmErrorReport      = UNQLITE_VM_CONFIG_ERR_REPORT
 , vmRecursionDepth   = UNQLITE_VM_CONFIG_RECURSION_DEPTH
 , vmOutputLength     = UNQLITE_VM_OUTPUT_LENGTH
 , vmCreateVar        = UNQLITE_VM_CONFIG_CREATE_VAR
 , vmHTTPRequest      = UNQLITE_VM_CONFIG_HTTP_REQUEST
 , vmServerAttributes = UNQLITE_VM_CONFIG_SERVER_ATTR
 , vmENVAttributes    = UNQLITE_VM_CONFIG_ENV_ATTR
 , vmExecValue        = UNQLITE_VM_CONFIG_EXEC_VALUE
 , vmIOStream         = UNQLITE_VM_CONFIG_IO_STREAM
 , vmARGVEntry        = UNQLITE_VM_CONFIG_ARGV_ENTRY
 , vmExtractOutput    = UNQLITE_VM_CONFIG_EXTRACT_OUTPUT
 }

-- | VM pointer
newtype VMp = VMp (Ptr ())
  deriving (Storable, Eq, Show)

-- | UnQLIte value
newtype UnQLiteValue = UnQLiteValue (Ptr ())
  deriving (Storable, Eq)

-- | Jx9Script type
type Jx9Script = CString
