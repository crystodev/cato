{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
module TokenUtils ( calculateTokensBalance, getTokenId, 
  getTokenPath, readTokensFromFile, recordTokens, saveMetadata, Token(..) ) where

import System.Directory ( doesFileExist)
import System.IO ( hGetContents )
import System.Process ( createProcess, proc, std_out, StdStream(CreatePipe), waitForProcess )
import Data.Aeson (FromJSON, ToJSON, toEncoding, genericToEncoding, decode, encode)
import Data.Aeson.TH(deriveJSON, defaultOptions, Options(fieldLabelModifier))
import GHC.Generics
import qualified Data.ByteString.Lazy.Char8 as B8
import qualified Data.Map as M
import Control.Monad( unless, forM_)
import Data.Maybe ( isJust, fromJust )
--import Data.List (isPrefixOf)
import Policy ( Policy(..), tokensPathName )

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
} deriving (Show, Eq)

-- record token minting
recordToken :: Policy -> String -> IO ()
recordToken policy tokenName = do
  let tokenFile = tokensPath policy ++ tokenName
  rc <- doesFileExist tokenFile
  unless rc $ do
    let id = getTokenId (policyId (policy:: Policy)) tokenName
    let tokenInfo = TokenInfo { infoVersion = tokenInfoVersion, name = tokenName, id = id, policyName = policyName (policy:: Policy), policyId = policyId (policy:: Policy)}
    B8.writeFile tokenFile (encode tokenInfo)

recordTokens :: Policy -> [String] -> IO ()
recordTokens policy = mapM_ (recordToken policy)

-- get token path
getTokenPath :: FilePath -> String -> FilePath
getTokenPath policyPath tokenName = policyPath ++ tokensPathName ++ tokenName

getTokenId :: String -> String -> String
getTokenId policyId tokenName = policyId ++ "." ++ tokenName

-- read tokens and amount from json file
readTokensFromFile :: Maybe FilePath -> [Token]
readTokensFromFile Nothing = []
readTokensFromFile (Just tokensFileName) = []

-- compute total balance for each tokens in list [(token,amount)]
calculateTokensBalance :: [(String, Int)] -> [(String, Int)] 
calculateTokensBalance tokensAmount = M.toList $ M.fromListWith (+) tokensAmount

-- metadata helpers --------------------------------------------------------------------

metadataFile = "/tmp/metadata.json"
saveMetadata :: String -> IO FilePath
saveMetadata jsonData = do 
  writeFile metadataFile jsonData
  return metadataFile
