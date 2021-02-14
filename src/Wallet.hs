module Wallet ( getAddressPath, Owner(..) ) where

newtype Owner = Owner String
            deriving (Eq, Show)

-- compute address path from addresses path and owner name
getAddressPath:: FilePath -> Owner -> FilePath
getAddressPath addressesPath ownerName = addressesPath ++ show ownerName ++ "/"
