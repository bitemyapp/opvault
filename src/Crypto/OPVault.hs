{-# LANGUAGE OverloadedStrings #-}
module Crypto.OPVault where

import Prelude hiding (length, readFile)
import Data.ByteString.Char8 (length, readFile)

import Crypto.OPVault.Common
import Crypto.OPVault.Encryption
import Crypto.OPVault.Types

result :: ResultT IO ()
result = do
    let path = "/home/crough/Dropbox/1Password/demo.opvault/default"
    profile <- fromJSON =<< io (readFile $ path ++ "/profile.js")

    let password = "demo"
    let derKey   = derivedKey profile password
    mk <- masterKey profile derKey

    band0 <- fromJSON =<< io (readFile $ path ++ "/band_0.js")
    let item = head $ toList (band0 :: ItemMap)
    ik <- itemKey item mk

    io . print =<< itemData item ik

main' :: IO ()
main' = runResultT result >>= \x ->
    case x of
      Left err -> putStr "ERROR:" >> putStrLn err
      Right () -> putStrLn "\nDone!"
