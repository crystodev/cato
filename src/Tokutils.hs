{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
module Tokutils ( buildPolicyName, createKeypair, createPolicy, Address, AddressType(Payment, Stake), 
  BlockchainNetwork(BlockchainNetwork, network, networkMagic, networkEra, networkEnv), 
  calculateTokensBalance, getPolicy, getPolicyPath, getPolicyId, getPolicyIdFromTokenId, Policy(..), 
  getProtocolKeyDeposit, saveProtocolParameters, getAddress, getAddressFile, getSkeyFile, getVkeyFile, recordToken, uglyParse ) where

import System.Directory ( createDirectoryIfMissing, doesFileExist)
import System.FilePath ( takeDirectory )
import System.IO ( hGetContents, readFile)
import System.Process ( createProcess, proc, std_out, StdStream(CreatePipe), waitForProcess )
import Data.Aeson (decode, encode, Object)
import Data.Aeson.TH(deriveJSON, defaultOptions, Options(fieldLabelModifier))
import GHC.Generics
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Map as M
import Control.Monad(when, unless)
import Data.Maybe ( isNothing, isJust, fromJust )
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
  , policyVkey   :: FilePath
  , policySkey   :: FilePath
  , tokensPath   :: FilePath
  , policyId     :: String
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
buildPolicyPath policyName policyPath = Policy 
  (policyPath ++ "policy.script")
  (policyPath ++ "policy.vkey")
  (policyPath ++ "policy.skey")
  (policyPath ++ "tokens/")
  ""

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
    let runParams = ["address", "key-gen", "--verification-key-file", policyVkey policy, "--signing-key-file", policySkey policy]
    (_, Just rc, _, ph) <- createProcess (proc "cardano-cli" runParams ){ std_out = CreatePipe }
    r <- waitForProcess ph
    -- create hash
    let runParams2 = ["address", "key-hash", "--payment-verification-key-file", policyVkey policy]
    (_, Just hout, _, _) <- createProcess (proc "cardano-cli" runParams2){ std_out = CreatePipe }
    keyh <- hGetContents hout
    let keyhash = filter (/= '\n') keyh
    -- create policy script
    let myPolicyScript = PolicyScript { keyHash = keyhash, keyType = "sig"}
    B.writeFile (policyScript policy) (encode myPolicyScript)
  else do
    putStrLn $ "Policy exists : no policy created for " ++ policyName
  -- retrieve policy script
  let runParams3 =["transaction", "policyid", "--script-file", policyScript policy]
  (_, Just hout, _, _) <- createProcess (proc "cardano-cli" runParams3){ std_out = CreatePipe }
  pId <- hGetContents hout 
  return (Just policy { policyId = filter (/= '\n') pId} )

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
    (_, Just hout, _, ph) <- createProcess (proc "cardano-cli" ["transaction", "policyid", "--script-file", policyScript policy]){ std_out = CreatePipe }
    r <- waitForProcess ph
    policyId <- hGetContents hout
    return $ Just $ Policy (policyScript policy) (policyVkey policy) (policySkey policy) (tokensPath policy) (filter (/= '\n') policyId)
  else
    return Nothing

-- record token minting
recordToken :: Policy -> String -> IO ()
recordToken policy tokenName = do
  let tokenFile = tokensPath policy ++ tokenName
  print tokenFile
  rc <- doesFileExist tokenFile
  print rc
  unless rc $ do
    print (policyId policy ++ "." ++ tokenName)
    writeFile tokenFile (policyId policy ++ "." ++ tokenName)
    
-- protocols helpers ------------------------------------------------------

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

  let keyDeposit = read (uglyParse jsonData "keyDeposit")::Int
  return (Just keyDeposit)

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

-- create keypair based on address_name
createKeypair :: AddressType -> FilePath -> String -> IO Bool
createKeypair addressType addressesPath ownerName = do
  let vkeyFile = getVkeyFile addressesPath addressType ownerName
  let skeyFile = getSkeyFile addressesPath addressType ownerName
  bool <- doesFileExist vkeyFile
  if bool then do
    putStrLn $ "key pair already exists for " ++ ownerName
    return False
  else do
    createDirectoryIfMissing True (takeDirectory vkeyFile)
    let saddressType = if addressType == Payment then "address" else "stake-address"
    
    (_, Just rc, _, ph) <- createProcess (proc "cardano-cli" [saddressType, "key-gen", "--verification-key-file", vkeyFile, "--signing-key-file", skeyFile]){ std_out = CreatePipe }
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
  let saddressType = if addressType == Payment then "payment" else "stake"
  let extmap = [ ("address", ".addr"), ("signing_key", ".skey"), ("verification_key", ".vkey")]
  let extm = lookup addressKey extmap
  case extm of
    Just ext -> getAddressPath addressesPath name ++ saddressType ++ name ++ ext
    _ -> ""

-- give file name for name type address
getAddressFile :: FilePath -> AddressType -> String -> FilePath
getAddressFile addressesPath addressType = getAddressKeyFile addressesPath addressType "address"

-- give file name for name type signing key
getSkeyFile :: FilePath -> AddressType -> String -> FilePath
getSkeyFile addressesPath addressType = getAddressKeyFile addressesPath addressType "signing_key"

-- give file name for name type verification key
getVkeyFile :: FilePath -> AddressType -> String -> FilePath
getVkeyFile addressesPath addressType = getAddressKeyFile addressesPath addressType "verification_key"
