-- address helpers ---------------------------------------------------------------------
module Address ( createKeyPair, Address (..), AddressType(Payment, Stake), getAddress, getAddressFile, getSKeyFile, getVKeyFile, ) where

import           System.Directory (createDirectoryIfMissing, doesFileExist)
import           System.FilePath  (takeDirectory)
import           System.IO        (hGetContents)
import           System.Process   (StdStream (CreatePipe), createProcess, proc,
                                   std_out, waitForProcess)

import           Policy           (signingKeyExt, verificationKeyExt)
import           Wallet           (getAddressPath, Owner(..))

-- data AdressOrKey = address | signing_key | verification_key deriving (Read, Show, Eq)
newtype Address = Address String deriving (Eq, Show)
data AddressType = Payment | Stake deriving (Read, Show, Eq)

-- create key pair based on address_name
createKeyPair :: AddressType -> FilePath -> Owner -> IO Bool
createKeyPair addressType addressesPath ownerName = do
  let vKeyFile = getVKeyFile addressesPath addressType ownerName
  let sKeyFile = getSKeyFile addressesPath addressType ownerName
  bool <- doesFileExist vKeyFile
  if bool then do
    putStrLn $ "key pair already exists for " ++ show ownerName
    return False
  else do
    createDirectoryIfMissing True (takeDirectory vKeyFile)
    let sAddressType = if addressType == Payment then "address" else "stake-address"

    (_, Just rc, _, ph) <- createProcess (proc "cardano-cli" [sAddressType, "key-gen", "--verification-key-file", vKeyFile, "--signing-key-file", sKeyFile]){ std_out = CreatePipe }
    r <- waitForProcess ph
    return True

-- TODO GetAddress
getAddress :: FilePath -> IO (Maybe Address)
getAddress addressFileName = do
  bool <- doesFileExist addressFileName
  if not bool then do
    putStrLn $ "file not found : " ++ addressFileName
    return Nothing
  else do
    addr <- readFile addressFileName
    return (Just $ Address addr)

-- give file name for name type address or key
getAddressKeyFile :: FilePath -> AddressType -> String -> Owner -> FilePath
getAddressKeyFile addressesPath addressType addressKey name = do
  let sAddressType = if addressType == Payment then "payment" else "stake"
  let extMap = [ ("address", ".addr"), ("signing_key", signingKeyExt), ("verification_key", verificationKeyExt)]
  let extM = lookup addressKey extMap
  case extM of
    Just ext -> getAddressPath addressesPath name ++ sAddressType ++ show name ++ ext
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
