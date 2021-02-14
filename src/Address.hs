-- address helpers ---------------------------------------------------------------------
module Address ( createKeyPair, Address (..), AddressType(Payment, Stake), getAddressFromFile, getAddressFile, getSKeyFile, getVKeyFile, ) where

import           System.Directory (createDirectoryIfMissing, doesFileExist)
import           System.FilePath  (takeDirectory)
import           System.IO        (hGetContents)
import           System.Process   (StdStream (CreatePipe), createProcess, proc,
                                   std_out, waitForProcess)

import           Policy           (signingKeyExt, verificationKeyExt)
import           Wallet           (getAddressPath, Owner(..))

-- data AdressOrKey = address | signing_key | verification_key deriving (Read, Show, Eq)
newtype Address = Address { getAddress :: String } deriving (Eq)

data AddressType = Payment | Stake deriving (Read, Show, Eq)

-- create key pair based on address_name
createKeyPair :: AddressType -> FilePath -> Owner -> IO Bool
createKeyPair addressType addressesPath owner = do
  let vKeyFile = getVKeyFile addressesPath addressType owner
  let sKeyFile = getSKeyFile addressesPath addressType owner
  bool <- doesFileExist vKeyFile
  if bool then do
    putStrLn $ "key pair already exists for " ++ getOwner owner
    return False
  else do
    createDirectoryIfMissing True (takeDirectory vKeyFile)
    let sAddressType = if addressType == Payment then "address" else "stake-address"

    (_, Just rc, _, ph) <- createProcess (proc "cardano-cli" [sAddressType, "key-gen", "--verification-key-file", vKeyFile, "--signing-key-file", sKeyFile]){ std_out = CreatePipe }
    r <- waitForProcess ph
    return True

-- get address from file
getAddressFromFile :: FilePath -> IO (Maybe Address)
getAddressFromFile addressFileName = do
  bool <- doesFileExist addressFileName
  if not bool then do
    putStrLn $ "file not found : " ++ addressFileName
    return Nothing
  else do
    addr <- readFile addressFileName
    return (Just $ Address addr)

-- give file name for name type address or key
getAddressKeyFile :: FilePath -> AddressType -> String -> Owner -> FilePath
getAddressKeyFile addressesPath addressType addressKey owner = do
  let sAddressType = if addressType == Payment then "payment" else "stake"
  let extMap = [ ("address", ".addr"), ("signing_key", signingKeyExt), ("verification_key", verificationKeyExt)]
  let extM = lookup addressKey extMap
  case extM of
    Just ext -> getAddressPath addressesPath owner ++ sAddressType ++ getOwner owner ++ ext
    _ -> ""

-- give file name for name type address
getAddressFile :: FilePath -> AddressType -> Owner -> FilePath
getAddressFile addressesPath addressType = getAddressKeyFile addressesPath addressType "address"

-- give file name for name type signing key
getSKeyFile :: FilePath -> AddressType -> Owner -> FilePath
getSKeyFile addressesPath addressType = getAddressKeyFile addressesPath addressType "signing_key"

-- give file name for name type verification key
getVKeyFile :: FilePath -> AddressType -> Owner -> FilePath
getVKeyFile addressesPath addressType = getAddressKeyFile addressesPath addressType "verification_key"
