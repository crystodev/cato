{-# LANGUAGE DeriveGeneric #-}
module Network ( BlockchainNetwork(BlockchainNetwork, network, networkMagic, networkEra, networkEnv), 
        getNetworkMagic, getNetworkEra, Tip(..) ) where


import Data.Aeson (FromJSON, ToJSON, toEncoding, genericToEncoding, decode, encode)
import Data.Aeson.TH(deriveJSON, defaultOptions, Options(fieldLabelModifier))
import GHC.Generics
-- import qualified Data.ByteString.Lazy.Char8 as B8
-- import qualified Data.Map as M
import Control.Monad( unless, forM_)
import Data.Maybe ( isJust, fromJust )

-- blockchain parameters
data BlockchainNetwork = BlockchainNetwork 
  {
    network :: String
  , networkMagic :: Int
  , networkEra :: Maybe String
  , networkEnv :: String
  }
  deriving Show

getNetworkMagic :: BlockchainNetwork -> [String]
getNetworkMagic bNetwork = [show (networkMagic bNetwork)]

getNetworkEra :: BlockchainNetwork -> [String]
getNetworkEra bNetwork = ["--" ++ fromJust (networkEra bNetwork) | isJust $ networkEra bNetwork]

data Tip = Tip {
  blockNo :: Int
, headerHash :: String
, slotNo :: Int
} deriving (Generic)
instance FromJSON Tip