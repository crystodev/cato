module Wallet ( getAddressPath ) where

-- compute address path from addresses path and owner name
getAddressPath:: FilePath -> String -> FilePath
getAddressPath addressesPath ownerName = addressesPath ++ ownerName ++ "/"
