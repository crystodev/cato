{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
module TokenUtils ( calculateTokensBalance, getTokenId, 
  getTokenPath, readTokensFromFile, recordTokens, saveMetadata, Token(..), writeTokensToFile ) where

import System.Directory ( doesFileExist)
import System.IO ( hGetContents )
import System.Process ( createProcess, proc, std_out, StdStream(CreatePipe), waitForProcess )
import Data.Aeson (FromJSON, ToJSON, toEncoding, genericToEncoding, decode, encode)
import Data.Aeson.TH(deriveJSON, defaultOptions, Options(fieldLabelModifier))
import GHC.Generics
import qualified Data.ByteString.Lazy.Char8 as B8
import qualified Data.Map as M
import Control.Monad( when, unless, forM_)
import Data.Maybe ( isJust, fromJust )
import Policy ( Policy(..), getPolicy, getPolicyIdFromTokenId, tokensPathName )

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

-- token and amount
data Token = Token {
  tokenName :: String
, tokenAmount :: Int
, tokenId :: String
, tokenPolicyName :: String
} deriving (Generic, Show, Eq)
instance FromJSON Token
instance ToJSON Token where
  toEncoding = genericToEncoding defaultOptions

newtype Tokens = Tokens {
  tokens :: [Token]
} deriving (Generic, Show, Eq)
instance FromJSON Tokens
instance ToJSON Tokens where
  toEncoding = genericToEncoding defaultOptions

-- record token minting
recordToken :: String -> Token -> IO ()
recordToken policiesPath token = do
  let polId = getPolicyIdFromTokenId (tokenId token)
  mPolicy <- getPolicy policiesPath (tokenPolicyName token)
  when (isJust mPolicy) $ do
    let policy = fromJust mPolicy
    let tokenFile = tokensPath policy ++ tokenName token
    rc <- doesFileExist tokenFile
    unless rc $ do
      let id = getTokenId (policyId (policy::Policy)) (tokenName token)
      let tokenInfo = TokenInfo { infoVersion = tokenInfoVersion, name = tokenName token, id = id, 
                                policyName = policyName (policy:: Policy), policyId = policyId (policy:: Policy)}
      B8.writeFile tokenFile (encode tokenInfo)

recordTokens :: String -> [Token] -> IO ()
recordTokens policiesPath = mapM_ (recordToken policiesPath)

-- get token path
getTokenPath :: FilePath -> String -> String -> FilePath
getTokenPath policiesPath policyName tokenName = policiesPath ++ policyName ++ "/" ++ tokensPathName ++ tokenName

getTokenId :: String -> String -> String
getTokenId policyId tokenName = policyId ++ "." ++ tokenName

-- read tokens and amount from json file
readTokensFromFile :: Maybe FilePath -> IO [Token]
readTokensFromFile Nothing = return []
-- TODO : read data from file
readTokensFromFile (Just tokensFileName) = do
  rc <- doesFileExist tokensFileName
  if rc then do
    jsonData <- readFile tokensFileName
    let mTokens = decode (B8.pack jsonData) :: Maybe Tokens
    if isJust mTokens then do
      let tokenList = tokens (fromJust mTokens)
      return tokenList
    else
      return []
  else return []

-- write tokens and amount to json file
writeTokensToFile :: [Token] -> FilePath -> IO ()
writeTokensToFile tokenList tokensFileName = do
  B8.writeFile tokensFileName (encode Tokens { tokens = tokenList })

-- compute total balance for each tokens in list [(token,amount)]
calculateTokensBalance :: [(String, Int)] -> [(String, Int)] 
calculateTokensBalance tokensAmount = M.toList $ M.fromListWith (+) tokensAmount

-- metadata helpers --------------------------------------------------------------------

metadataFile = "/tmp/metadata.json"
saveMetadata :: String -> IO FilePath
saveMetadata jsonData = do 
  writeFile metadataFile jsonData
  return metadataFile
