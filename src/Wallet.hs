module Wallet ( getAddressPath, Owner(..) ) where

import           Baseutils            (capitalized)
newtype Owner = Owner { getOwner :: String }
            deriving (Eq)

-- compute address path from addresses path and owner name
getAddressPath:: FilePath -> Owner -> FilePath
getAddressPath addressesPath owner = addressesPath ++ capitalized (getOwner owner) ++ "/"
