module Crypto.OPVault.Common.Base64
    ( Base64 (rawBytes)
    ) where

import Control.Monad (mzero)
import Data.Aeson (FromJSON(..), Value(String))
import Data.ByteString.Char8 (ByteString, unpack)
import qualified Data.ByteString.Base64 as B64 (encode, decode)
import Data.Text.Encoding (encodeUtf8)

newtype Base64 = Base64 { rawBytes :: ByteString } deriving Eq

instance Show Base64 where
    show (Base64 bStr) = "B64: " ++ unpack (B64.encode bStr)

instance FromJSON Base64 where
    parseJSON (String txt) =
        case B64.decode $ encodeUtf8 txt of
            Left _    -> mzero
            Right raw -> return $ Base64 raw
    parseJSON _ = mzero
