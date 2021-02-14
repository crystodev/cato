module Wallet ( getAddressPath, Owner(..) ) where

newtype Owner = Owner { getOwner :: String }
            deriving (Eq)

-- compute address path from addresses path and owner name
getAddressPath:: FilePath -> Owner -> FilePath
getAddressPath addressesPath ownerName = addressesPath ++ getOwner ownerName ++ "/"
