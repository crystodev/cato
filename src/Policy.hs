-- policy helpers ---------------------------------------------------------
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
module Policy ( buildPolicyName, createPolicy, getPolicy, getPolicies, getPoliciesPath, getPolicyId, getPolicyIdFromTokenId,
    Policy(..), tokensPathName, signingKeyExt, verificationKeyExt ) where

import           Data.Aeson                 (FromJSON, ToJSON, decode, encode,
                                             genericToEncoding, toEncoding)
import           Data.Aeson.TH              (Options (fieldLabelModifier),
                                             defaultOptions, deriveJSON)
import qualified Data.ByteString.Lazy.Char8 as B8
import           Data.List                  (nub)
import           Data.List.Split            (splitOn)
import           Data.Maybe                 (fromJust, isJust)
import           GHC.Generics
import           System.Directory           (createDirectoryIfMissing,
                                             doesFileExist)
import           System.FilePath            (takeDirectory)
import           System.IO                  (hGetContents)
import           System.Process             (StdStream (CreatePipe),
                                             createProcess, proc, std_out,
                                             waitForProcess)
import           Wallet                     (getAddressPath, Owner(..))

data PolicyScript = PolicyScript
  {
    keyHash :: String
  , keyType :: String
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
  deriving (Show, Eq)

-- some constants
addressExt = ".addr"
scriptExt = ".script"
signingKeyExt = ".skey"
tokensPathName = "tokens/"
verificationKeyExt = ".vkey"

-- build policy name
-- if no policy name specified, search policy with token name
buildPolicyName :: String -> Maybe String -> String
buildPolicyName "" Nothing          = "noName"
buildPolicyName "" (Just tokenName) = tokenName
buildPolicyName policyName _        = policyName

-- build Policy full file names
buildPolicy :: String -> String -> Policy
buildPolicy policyName policyPath = Policy {
    policyScript = policyPath ++ policyName ++ "/" ++ "policy" ++ scriptExt
  , policyVKey = policyPath ++ policyName ++ "/" ++ "policy" ++ verificationKeyExt
  , policySKey = policyPath ++ policyName ++ "/" ++ "policy" ++ signingKeyExt
  , tokensPath = policyPath ++ policyName ++ "/" ++ tokensPathName
  , policyId =  ""
  , policyName = policyName
  }

-- create a Cardano Policy
createPolicy :: String -> String -> IO (Maybe Policy)
createPolicy policyName policyPath = do
  let policy = buildPolicy policyName policyPath
  -- check if policy script exists
  -- if so returns existing Policy
  bool <- doesFileExist (policyScript policy)
  if not bool then do
    createDirectoryIfMissing True (tokensPath policy)
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


-- | get Policies Folder
getPoliciesPath:: FilePath -> Owner -> FilePath -> FilePath
getPoliciesPath addressPath ownerName policiesFolder = getAddressPath addressPath ownerName ++ policiesFolder

-- get Policy Id
getPolicyId:: Policy -> String
getPolicyId = policyId

-- get policyId from token id
getPolicyIdFromTokenId :: String -> String
getPolicyIdFromTokenId tokenId = head (splitOn "." tokenId)

-- get policy by name
getPolicy :: FilePath -> String -> IO (Maybe Policy)
getPolicy policiesPath policyName = do
  let policy = buildPolicy policyName policiesPath
  bool <- doesFileExist (policyScript policy)
  if bool then do
    -- get policy id
    (_, Just hOut, _, ph) <- createProcess (proc "cardano-cli" ["transaction", "policyid", "--script-file", policyScript policy]){ std_out = CreatePipe }
    r <- waitForProcess ph
    policyId <- hGetContents hOut
    return $ Just $ Policy (policyScript policy) (policyVKey policy) (policySKey policy) (tokensPath policy) (filter (/= '\n') policyId) policyName
  else
    return Nothing

-- get policy list (removing duplicates)
getPolicies :: FilePath -> [String] -> IO [Policy]
getPolicies policiesPath policyNames = do
  mPolicies <- mapM (getPolicy  policiesPath) policyNames
  let policies = nub [fromJust mPolicy | mPolicy <- mPolicies, isJust mPolicy]
  return policies
