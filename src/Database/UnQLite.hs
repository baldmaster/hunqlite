{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Database.UnQLite where
import Database.UnQLite.Bindings
import Database.UnQLite.Types
import qualified Database.UnQLite.Internal as Internal

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.ForeignPtr
import Data.ByteString
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Text (Text)
import Control.Exception
import Data.Typeable
import qualified Foreign.Concurrent as Conc

-- | UnQLite error
data UnQLiteError = UnQLiteError
    { errorCode      :: !StatusCode
        -- ^ Error code returned by API call
    , errorMessage   :: Text
        -- ^ Text describing the error
    , errorContext   :: Text
        -- ^ Indicates action that caused the error
    }
    deriving (Eq, Typeable)


instance Show UnQLiteError where
    show UnQLiteError { errorCode   = code
                      , errorMessage = message
                      , errorContext = context
                      }
      = T.unpack $ T.concat
         [ "UnQLite returned "
         , T.pack $ show code
         , " while attempting to perform "
         , context
         , ": "
         , message
         ]

instance Exception UnQLiteError

throwUnQLiteError :: StatusCode -> Text -> Text -> IO a
throwUnQLiteError code message context = do
    throwIO UnQLiteError
        { errorCode        = code
        , errorMessage     = message
        , errorContext     = context
        }

checkError :: Text -> Either (StatusCode, Text) a -> IO a
checkError context result = case result of
    Left (err, msg) -> throwUnQLiteError err msg context
    Right a         -> return a


newUnQLiteHandle :: UnQLite -> IO UnQLiteHandle
newUnQLiteHandle h@(UnQLite p) = UnQLiteHandle `fmap` Conc.newForeignPtr p close
  where close = c_unqlite_close h >> return ()

-- | Open connection
open :: Text -> CAccessMode -> IO UnQLite
open dbName mode =
  Internal.open dbName mode >>= checkError "open"

openHandle :: Text -> CAccessMode -> IO UnQLiteHandle
openHandle dbName mode =
  open dbName mode >>= newUnQLiteHandle

-- | Close connection
close conn = Internal.close conn >>= checkError "close"

-- | Close handle
closeHandle (UnQLiteHandle h) = do
  withForeignPtr h $ \p -> close (UnQLite p)

-- | Write a new record into the database.
-- If the record does not exists, it is created. Otherwise, it is replaced.
kvStore :: UnQLiteHandle -> ByteString -> ByteString -> IO ()
kvStore (UnQLiteHandle h) k v = do
  Internal.kvStore h k v >>= checkError "store"

-- | Write a new record into the database.
-- If the record does not exists, it is created.
-- Otherwise, the new data chunk is appended to the end of the old chunk.
kvAppend :: UnQLiteHandle -> ByteString -> ByteString -> IO ()
kvAppend (UnQLiteHandle h) k v = do
  Internal.kvAppend h k v >>= checkError "append"

-- | Fetch a record from the database.
kvFetch :: UnQLiteHandle -> ByteString -> IO ByteString
kvFetch (UnQLiteHandle h) k = do
  Internal.kvFetch h k >>= checkError "fetch"

-- | Remove a record from the database.
kvDelete :: UnQLiteHandle -> ByteString -> IO ()
kvDelete (UnQLiteHandle h) k = do
  Internal.kvDelete h k >>= checkError "delete"
