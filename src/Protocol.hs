-- protocols helpers ------------------------------------------------------
{-# LANGUAGE DeriveGeneric #-}
module Protocol ( getProtocolKeyDeposit, getProtocolMinUTxOValue, saveProtocolParameters ) where

import           Data.Aeson                 (FromJSON, ToJSON, decode, encode,
                                             genericToEncoding, toEncoding)
import           Data.Aeson.TH              (Options (fieldLabelModifier),
                                             defaultOptions, deriveJSON)
import qualified Data.ByteString.Lazy.Char8 as B8
import           Data.Maybe                 (fromJust, isJust)
import           GHC.Generics
import           Network                    (BlockchainNetwork (..),
                                             getNetworkEra, getNetworkMagic)
import           System.IO                  (hGetContents)
import           System.Process             (StdStream (CreatePipe),
                                             createProcess, proc, std_out,
                                             waitForProcess)

-- protocol parameters json structure
data ProtocolVersion = ProtocolVersion {
  minor :: Int
, major :: Int
}  deriving (Generic, Show)
instance FromJSON ProtocolVersion

newtype ExtraEntropy = ExtraEntropy {
  tag :: String
}  deriving (Generic, Show)
instance FromJSON ExtraEntropy

data ProtocolParams = ProtocolParams {
  poolDeposit           :: Int
, protocolVersion       :: ProtocolVersion
, minUTxOValue          :: Int
, decentralisationParam :: Float
, maxTxSize             :: Int
, minPoolCost           :: Int
, minFeeA               :: Int
, maxBlockBodySize      :: Int
, minFeeB               :: Int
, eMax                  :: Int
, extraEntropy          :: ExtraEntropy
, maxBlockHeaderSize    :: Int
, keyDeposit            :: Int
, nOpt                  :: Int
, rho                   :: Float
, tau                   :: Float
, a0                    :: Float
} deriving (Generic, Show)
instance FromJSON ProtocolParams



-- get protocol parameters
getProtocolParams :: BlockchainNetwork -> IO (Maybe ProtocolParams)
getProtocolParams bNetwork = do
  let netName = network bNetwork
  let netMagic = getNetworkMagic bNetwork
  let netEra = getNetworkEra bNetwork
  let envParam = networkEnv bNetwork
  let runParams = ["query", "protocol-parameters", netName]++netMagic++netEra
  (_, Just rc, _, ph) <- createProcess (proc "cardano-cli" runParams ){ std_out = CreatePipe }
  r <- waitForProcess ph
  jsonData <- hGetContents rc
  let protocolParams = decode (B8.pack jsonData) :: Maybe ProtocolParams
  return protocolParams

-- get minUTxOValue parameter from protocol
getProtocolMinUTxOValue :: BlockchainNetwork -> IO (Maybe Int)
getProtocolMinUTxOValue bNetwork = do
  protocolParams <- getProtocolParams bNetwork
  if isJust protocolParams then return $ Just $ minUTxOValue $ fromJust protocolParams else return Nothing

-- get keyDeposit parameter from protocol
getProtocolKeyDeposit :: BlockchainNetwork -> IO (Maybe Int)
getProtocolKeyDeposit bNetwork = do
  protocolParams <- getProtocolParams bNetwork
  if isJust protocolParams then return $ Just $ keyDeposit $ fromJust protocolParams else return Nothing
  -- protocol params changed : no more keyDeposit available
  -- so replace temporarily with hard value
  -- return $ Just 2000000

-- get protocol parameters
saveProtocolParameters :: BlockchainNetwork -> FilePath -> IO Bool
saveProtocolParameters bNetwork protocolParams = do
  let netName = network bNetwork
  let netMagic = getNetworkMagic bNetwork
  let netEra = getNetworkEra bNetwork
  let envParam = networkEnv bNetwork
  let runParams = ["query", "protocol-parameters", netName]++netMagic++netEra++["--out-file", protocolParams]
  (_, Just rc, _, ph) <- createProcess (proc "cardano-cli" runParams){ std_out = CreatePipe }
  r <- waitForProcess ph
  return True
