{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Crypto.OPVault where

import Prelude hiding (readFile)
import Data.ByteString.Char8 (readFile)

import Crypto.OPVault.Common
import Crypto.OPVault.Encryption
import Crypto.OPVault.Types

result :: ResultT IO ()
result = do
    let path = "/home/crough/Dropbox/1Password/demo.opvault/default"
    profile@Profile{..} <- fromJSON =<< io (readFile $ path ++ "/profile.js")

    let password = "demo"
    let derKey   = derivedKey profile password
    mk <- flip masterKey derKey =<< opdata pMasterKey

    band0 <- fromJSON =<< io (readFile $ path ++ "/band_0.js")
    let i@Item{..} = head $ toList (band0 :: ItemMap)

    io $ print iUUID

main' :: IO ()
main' = runResultT result >>= \x ->
    case x of
      Left err -> putStrLn err
      Right () -> putStrLn "\nDone!"
