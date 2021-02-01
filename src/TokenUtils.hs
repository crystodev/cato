{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
module TokenUtils ( buildPolicyName, createKeyPair, createPolicy, Address, AddressType(Payment, Stake), 
  BlockchainNetwork(BlockchainNetwork, network, networkMagic, networkEra, networkEnv), 
  calculateTokensBalance, getPolicy, getPolicyPath, getPolicyId, getPolicyIdFromTokenId, Policy(..), 
  getProtocolKeyDeposit, saveProtocolParameters, getAddress, getAddressFile, getSKeyFile, getVKeyFile, recordToken, uglyParse ) where

import System.Directory ( createDirectoryIfMissing, doesFileExist)
import System.FilePath ( takeDirectory )
import System.IO ( hGetContents )
import System.Process ( createProcess, proc, std_out, StdStream(CreatePipe), waitForProcess )
import Data.Aeson (FromJSON, ToJSON, toEncoding, genericToEncoding, decode, encode)
import Data.Aeson.TH(deriveJSON, defaultOptions, Options(fieldLabelModifier))
import GHC.Generics
import qualified Data.ByteString.Lazy.Char8 as B8
import qualified Data.Map as M
import Control.Monad( unless)
import Data.Maybe ( isJust, fromJust )
import Data.List.Split ( splitOn )
import Data.List (isPrefixOf)

-- policy helpers ---------------------------------------------------------
data PolicyScript = PolicyScript
  { 
    keyHash :: String
  , keyType    :: String
  }
  deriving (Show, Generic)

$(deriveJSON defaultOptions{fieldLabelModifier = \f -> if f == "keyType" then "type" else f} ''PolicyScript)

-- Cardano Policy
data Policy = Policy
  { 
    policyScript :: FilePath
  , policyVKey   :: FilePath
  , policySKey   :: FilePath
  , tokensPath   :: FilePath
  , policyId     :: String
  , policyName   :: String
  }
  deriving Show

-- build policy name
-- if no policy name specified, search policy with token name
buildPolicyName :: String -> Maybe String -> String
buildPolicyName "" Nothing = "noName"
buildPolicyName "" (Just tokenName) = tokenName
buildPolicyName policyName _ = policyName

-- build Policy full file names
buildPolicyPath :: String -> String -> Policy
buildPolicyPath policyName policyPath = Policy {
    policyScript = policyPath ++ "policy.script"
  , policyVKey = policyPath ++ "policy.vkey"
  , policySKey = policyPath ++ "policy.skey"
  , tokensPath = policyPath ++ "tokens/"
  , policyId =  ""
  , policyName = ""
  }

-- create a Cardano Policy
createPolicy :: String -> String -> IO (Maybe Policy)
createPolicy policyName policyPath = do
  let policy = buildPolicyPath policyName policyPath
  -- check if policy script exists
  -- if so returns existing Policy
  bool <- doesFileExist (policyScript policy)
  if not bool then do
    createDirectoryIfMissing True policyPath
    -- create policy key files
    let runParams = ["address", "key-gen", "--verification-key-file", policyVKey policy, "--signing-key-file", policySKey policy]
    (_, Just rc, _, ph) <- createProcess (proc "cardano-cli" runParams ){ std_out = CreatePipe }
    r <- waitForProcess ph
    -- create hash
    let runParams2 = ["address", "key-hash", "--payment-verification-key-file", policyVKey policy]
    (_, Just hOut, _, _) <- createProcess (proc "cardano-cli" runParams2){ std_out = CreatePipe }
    keyH <- hGetContents hOut
    let keyHash = filter (/= '\n') keyH
    -- create policy script
    let myPolicyScript = PolicyScript { keyHash = keyHash, keyType = "sig"}
    B8.writeFile (policyScript policy) (encode myPolicyScript)
  else do
    putStrLn $ "Policy exists : no policy created for " ++ policyName
  -- retrieve policy script
  let runParams3 =["transaction", "policyid", "--script-file", policyScript policy]
  (_, Just hOut, _, _) <- createProcess (proc "cardano-cli" runParams3){ std_out = CreatePipe }
  pId <- hGetContents hOut 
  return (Just (policy { policyId = filter (/= '\n') pId, policyName = policyName}::Policy ))

-- | get Policy Folder
getPolicyPath:: FilePath -> String -> String -> FilePath -> FilePath
getPolicyPath addressPath ownerName "" policiesFolder = getAddressPath addressPath ownerName ++ policiesFolder
getPolicyPath addressPath ownerName policyName policiesFolder = getAddressPath addressPath ownerName ++ policiesFolder ++ policyName ++ "/"

-- get Policy Id
getPolicyId:: Policy -> String
getPolicyId = policyId

-- get policyId from token id
getPolicyIdFromTokenId :: String -> String
getPolicyIdFromTokenId tokenId = head (splitOn "." tokenId)

-- get policy
getPolicy :: String -> FilePath -> IO (Maybe Policy)
getPolicy policyName policyPath = do
  let policy = buildPolicyPath policyName policyPath
  bool <- doesFileExist (policyScript policy)
  if bool then do 
    -- get policy id
    (_, Just hOut, _, ph) <- createProcess (proc "cardano-cli" ["transaction", "policyid", "--script-file", policyScript policy]){ std_out = CreatePipe }
    r <- waitForProcess ph
    policyId <- hGetContents hOut
    return $ Just $ Policy (policyScript policy) (policyVKey policy) (policySKey policy) (tokensPath policy) (filter (/= '\n') policyId) policyName
  else
    return Nothing

-- token info record
data TokenInfo = TokenInfo {
  infoVersion :: Float
, name :: String
, id :: String
, policyName :: String
, policyId :: String
} deriving (Generic, Show)

tokenInfoVersion = 0.1 :: Float

instance FromJSON TokenInfo

instance ToJSON TokenInfo where
  toEncoding = genericToEncoding defaultOptions

-- record token minting
recordToken :: Policy -> String -> IO ()
recordToken policy tokenName = do
  let tokenFile = tokensPath policy ++ tokenName
  rc <- doesFileExist tokenFile
  unless rc $ do
    let id = policyId (policy:: Policy)  ++ "." ++ tokenName
    let tokenInfo = TokenInfo { infoVersion = tokenInfoVersion, name = tokenName, id = id, policyName = policyName (policy:: Policy), policyId = policyId (policy:: Policy)}
    B8.writeFile tokenFile (encode tokenInfo)
    
-- protocols helpers ------------------------------------------------------

-- protocol parameters json structure
data ProtocolVersion = ProtocolVersion {
  minor :: Int
, major :: Int
}  deriving (Generic)
instance FromJSON ProtocolVersion

newtype ExtraEntropy = ExtraEntropy {
  tag :: String
}  deriving (Generic)
instance FromJSON ExtraEntropy

data ProtocolParams = ProtocolParams {
  protocolVersion :: ProtocolVersion
, minUTxOValue :: Int
, decentralisationParam :: Int
, maxTxSize :: Int
, minPoolCost :: Int
, minFeeA :: Int
, maxBlockBodySize :: Int
, minFeeB :: Int
, eMax :: Int
, extraEntropy :: ExtraEntropy
, maxBlockHeaderSize :: Int
, keyDeposit :: Int
, nOpt :: Int
, rho :: Float
, tau :: Float
, a0 :: Float
} deriving (Generic)
instance FromJSON ProtocolParams

-- blockchain parameters
data BlockchainNetwork = BlockchainNetwork 
  {
    network :: String
  , networkMagic :: Int
  , networkEra :: String
  , networkEnv :: String
  }
  deriving Show

-- get keyDeposit parameter from protocol
uglyParse :: String -> String -> String
uglyParse jsonData key = do
  let ls = splitOn "," (filter (`notElem` "\n\" {}") jsonData)
  last $ splitOn ":" (head ( filter (isPrefixOf key) ls))

getProtocolKeyDeposit :: BlockchainNetwork -> IO (Maybe Int)
getProtocolKeyDeposit bNetwork = do
  let netName = network bNetwork
  let netMagic = networkMagic bNetwork
  let netEra = networkEra bNetwork
  let envParam = networkEnv bNetwork
  (_, Just rc, _, ph) <- createProcess (proc "cardano-cli" ["query", "protocol-parameters", netName, show netMagic, netEra]){ std_out = CreatePipe }
  r <- waitForProcess ph
  jsonData <- hGetContents rc
  let protocolParams = decode (B8.pack jsonData) :: Maybe ProtocolParams
  if isJust protocolParams then return $ Just $ keyDeposit $ fromJust protocolParams else return Nothing
--  let keyDeposit = read (uglyParse jsonData "keyDeposit")::Int
--  return (Just keyDeposit)

-- get protocol parameters
saveProtocolParameters :: BlockchainNetwork -> FilePath -> IO Bool
saveProtocolParameters bNetwork protocolParams = do
  let netName = network bNetwork
  let netMagic = networkMagic bNetwork
  let netEra = networkEra bNetwork
  let envParam = networkEnv bNetwork
  (_, Just rc, _, ph) <- createProcess (proc "cardano-cli" ["query", "protocol-parameters", netName, show netMagic, netEra, "--out-file", protocolParams]){ std_out = CreatePipe }
  r <- waitForProcess ph
  return True

-- create key pair based on address_name
createKeyPair :: AddressType -> FilePath -> String -> IO Bool
createKeyPair addressType addressesPath ownerName = do
  let vKeyFile = getVKeyFile addressesPath addressType ownerName
  let sKeyFile = getSKeyFile addressesPath addressType ownerName
  bool <- doesFileExist vKeyFile
  if bool then do
    putStrLn $ "key pair already exists for " ++ ownerName
    return False
  else do
    createDirectoryIfMissing True (takeDirectory vKeyFile)
    let sAddressType = if addressType == Payment then "address" else "stake-address"
    
    (_, Just rc, _, ph) <- createProcess (proc "cardano-cli" [sAddressType, "key-gen", "--verification-key-file", vKeyFile, "--signing-key-file", sKeyFile]){ std_out = CreatePipe }
    r <- waitForProcess ph
    return True

-- compute total balance for each tokens in list [(token,amount)]
calculateTokensBalance :: [(String, Int)] -> [(String, Int)] 
calculateTokensBalance tokensAmount = M.toList $ M.fromListWith (+) tokensAmount

-- address helpers ---------------------------------------------------------------------

-- data AdressOrKey = address | signing_key | verification_key deriving (Read, Show, Eq)
type Address = String
data AddressType = Payment | Stake deriving (Read, Show, Eq)

-- TODO GetAddress
getAddress :: FilePath -> IO (Maybe Address)
getAddress addressFileName = do
  bool <- doesFileExist addressFileName
  if not bool then do
    putStrLn $ "file not found : " ++ addressFileName
    return Nothing
  else do 
    addr <- readFile addressFileName
    return (Just addr)

-- compute address path from addresses path and owner name
getAddressPath:: FilePath -> String -> FilePath
getAddressPath addressesPath ownerName = addressesPath ++ ownerName ++ "/"

-- give file name for name type address or key
getAddressKeyFile :: FilePath -> AddressType -> String -> String -> FilePath
getAddressKeyFile addressesPath addressType addressKey name = do
  let sAddressType = if addressType == Payment then "payment" else "stake"
  let extMap = [ ("address", ".addr"), ("signing_key", ".skey"), ("verification_key", ".vkey")]
  let extM = lookup addressKey extMap
  case extM of
    Just ext -> getAddressPath addressesPath name ++ sAddressType ++ name ++ ext
    _ -> ""

-- give file name for name type address
getAddressFile :: FilePath -> AddressType -> String -> FilePath
getAddressFile addressesPath addressType = getAddressKeyFile addressesPath addressType "address"

-- give file name for name type signing key
getSKeyFile :: FilePath -> AddressType -> String -> FilePath
getSKeyFile addressesPath addressType = getAddressKeyFile addressesPath addressType "signing_key"

-- give file name for name type verification key
getVKeyFile :: FilePath -> AddressType -> String -> FilePath
getVKeyFile addressesPath addressType = getAddressKeyFile addressesPath addressType "verification_key"
