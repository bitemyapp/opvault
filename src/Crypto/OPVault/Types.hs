{-# LANGUAGE OverloadedStrings #-}
module Crypto.OPVault.Types where

import Control.Monad (mzero)
import Data.String (IsString(..))
import Data.Text (unpack)

import Crypto.OPVault.Common

newtype Vault = VaultPath String deriving (Show, Eq)

instance IsString Vault where
    fromString = VaultPath

data Profile = Profile
    { pUuid          :: Text
    , pCreatedAt     :: Int
    , pUpdatedAt     :: Int
    , pLastUpdatedBy :: Text
    , pProfileName   :: Text
    , pPasswordHint  :: Text

    , pIterations    :: Int
    , pMasterKey     :: Base64
    , pOverviewKey   :: Base64
    , pSalt          :: Base64
    } deriving (Eq, Show)

instance FromJSON Profile where
    parseJSON (Object obj) =
        Profile                <$>
        obj .: "uuid"          <*>
        obj .: "createdAt"     <*>
        obj .: "updatedAt"     <*>
        obj .: "lastUpdatedBy" <*>
        obj .: "profileName"   <*>
        obj .: "passwordHint"  <*>
        obj .: "iterations"    <*>
        obj .: "masterKey"     <*>
        obj .: "overviewKey"   <*>
        obj .: "salt"

data Folder = Folder
    { fUUID     :: Text
    , fCreated  :: Int
    , fUpdated  :: Int
    , fTx       :: Int
    , fOverview :: Base64
    } deriving Show

instance FromJSON Folder where
    parseJSON (Object obj) =
        Folder           <$>
        obj .: "uuid"    <*>
        obj .: "created" <*>
        obj .: "updated" <*>
        obj .: "tx"      <*>
        obj .: "overview"

data Item = Item
    { iUUID     :: Text
    , iCategory :: Text
    , iCreated  :: Int
    , iUpdated  :: Int
    , iTx       :: Int

    , iHMAC     :: Base64
    , iDetails  :: Base64
    , iEncKey   :: Base64
    , iOverview :: Base64
    }

instance FromJSON Item where
    parseJSON (Object obj) =
        Item              <$>
        obj .: "uuid"     <*>
        obj .: "category" <*>
        obj .: "created"  <*>
        obj .: "updated"  <*>
        obj .: "tx"       <*>
        obj .: "hmac"     <*>
        obj .: "d"        <*>
        obj .: "k"        <*>
        obj .: "o"

data ItemDetails = ItemDetails
    { iBackupKeys :: [Base64]
    , iFields     :: [ItemField]
    , iSections   :: [ItemSection]
    } deriving (Show, Eq)

instance FromJSON ItemDetails where
    parseJSON (Object obj) =
        ItemDetails         <$>
        obj .: "backupKeys" <*>
        obj .: "fields"     <*>
        obj .: "sections"
    parseJSON _ = mzero

data ItemSection = ItemSection
    { iSectionName   :: Text
    , iSectionFields :: [Object]
    } deriving (Show, Eq)

instance FromJSON ItemSection where
    parseJSON (Object obj) =
        ItemSection <$>
        obj .: "name"   <*>
        obj .: "fields"
    parseJSON _ = mzero

data ItemField = ItemField
    { iDesignation :: Text
    , iName        :: Text
    , iType        :: FieldType
    , iValue       :: Text
    } deriving (Show, Eq)

instance FromJSON ItemField where
    parseJSON (Object obj) =
        ItemField            <$>
        obj .: "designation" <*>
        obj .: "name"        <*>
        obj .: "type"        <*>
        obj .: "value"
    parseJSON _ = mzero

data FieldType = UsernameField | PasswordField deriving (Show, Eq)

instance FromJSON FieldType where
    parseJSON (String s) = case s of
        "T" -> return UsernameField
        "P" -> return PasswordField
        _   -> mzero
    parseJSON _ = mzero

type ItemMap = HashMap Text Item
type FolderMap = HashMap Text Folder
