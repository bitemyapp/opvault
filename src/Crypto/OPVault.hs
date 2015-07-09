{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes        #-}
module Crypto.OPVault where

import Crypto.OPVault.FileSystem
import Crypto.OPVault.Types

result :: ResultT IO ()
result = undefined

main' :: IO ()
main' = runResultT result>>= \x ->
    case x of
      Left err -> putStr "ERROR:" >> putStrLn err
      Right () -> putStrLn "\nDone!"
