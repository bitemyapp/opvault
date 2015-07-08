{-# LANGUAGE OverloadedStrings #-}
module Crypto.OPVault.Types where

import Data.Text (unpack)

import Crypto.OPVault.Common

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
    }

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
--  , iFolder   :: Text
    , iCategory :: Text
    , iCreated  :: Int
    , iUpdated  :: Int
    , iTx       :: Int

    , iHMAC     :: Base64
    , iDetails  :: Base64
    , iKey      :: Base64
    , iOverview :: Base64
    }

instance FromJSON Item where
    parseJSON (Object obj) =
        Item              <$>
        obj .: "uuid"     <*>
--      obj .: "folder"   <*>
        obj .: "category" <*>
        obj .: "created"  <*>
        obj .: "updated"  <*>
        obj .: "tx"       <*>
        obj .: "hmac"     <*>
        obj .: "d"        <*>
        obj .: "k"        <*>
        obj .: "o"

type ItemMap = HashMap Text Item
type FolderMap = HashMap Text Folder
