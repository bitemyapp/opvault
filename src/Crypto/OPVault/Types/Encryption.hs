module Crypto.OPVault.Types.Encryption where

import Data.Text.Encoding (encodeUtf8)

import Crypto.OPVault.Types.Common

newtype Password = Password ByteString

instance Show Password where
    show = const "Password <...>"

instance IsString Password where
    fromString = Password . encodeUtf8 . fromString

data DerivedKey = DerivedKey
    { dKey :: ByteString
    , dMAC :: ByteString
    } deriving Eq

instance Show DerivedKey where
    show = const "DerivedKey <...>"

data MasterKey = MasterKey
    { mKey :: ByteString
    , mMAC :: ByteString
    }

instance Show MasterKey where
    show = const "MasterKey <...>"

data OverviewKey = OverviewKey
    { oKey :: ByteString
    , oMAC :: ByteString
    }

instance Show OverviewKey where
    show = const "OverviewKey <...>"

data ItemKey = ItemKey
    { iKey :: ByteString
    , iMAC :: ByteString
    }

instance Show ItemKey where
    show = const "ItemKey <...>"

