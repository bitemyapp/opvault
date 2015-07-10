module Crypto.OPVault.Types.ItemIndex where

import qualified Data.HashMap.Strict as HM (HashMap, lookup)
import Data.Hashable (Hashable(..))

import Crypto.OPVault.Types.Common
import Crypto.OPVault.Types.Encryption
import Crypto.OPVault.Types.FileTypes
import Crypto.OPVault.Types.ResultT

newtype ItemIndex = ItemIndex (HashMap IndexKey Text, HashMap Text Item)

data IndexKey
    = Title Text
    | UUID  Text
    deriving (Show, Eq)

instance Hashable IndexKey where
    hashWithSalt i x = hashWithSalt i $
        case x of Title x' -> x'
                  UUID  x' -> x'

lookupStr :: Text -> Object ->  Maybe Text
lookupStr key obj = case HM.lookup key obj of
    Just (String txt) -> Just txt
    _                 -> Nothing

itemLookup :: IndexKey -> ItemIndex -> Maybe Item
itemLookup (UUID x)     (ItemIndex (_, m)) = HM.lookup x m
itemLookup key      idx@(ItemIndex (m, _)) = flip itemLookup idx . UUID =<< HM.lookup key m
