{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Database.UnQLite.Types where

import Foreign
import Foreign.ForeignPtr
import Foreign.C.Types

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
