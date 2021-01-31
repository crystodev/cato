import System.Environment
import Options.Applicative
import Data.Semigroup ((<>))
import Data.Maybe ( isJust, fromJust )
import Baseutils ( capitalized )
import Control.Monad (void, when)
import Configuration.Dotenv (loadFile, defaultConfig)
import Tokutils ( AddressType(Payment, Stake), BlockchainNetwork(BlockchainNetwork, network, networkMagic, networkEra, networkEnv), createKeypair, createPolicy, getPolicyPath, getPolicyId )
import Transaction ( createAddress )

type Owner = String

data Address = Address {
  payment :: Bool
, stake :: Bool
, paymentKey :: Bool
, stakeKey :: Bool
} deriving Show

data Options = Options Owner Address

parseOwner :: Parser Owner
parseOwner = argument str (metavar "OWNER")

parseAddress :: Parser Address
parseAddress = Address
          <$> switch ( long "payment" <> short 'p' <> help "payment address" )
          <*> switch ( long "stake" <> short 's' <> help "stake address" )
          <*> switch ( long "payment-key" <> help "payment key pair address" )
          <*> switch ( long "stake-key" <> help "stake key pair address" )

parseOptions :: Parser Options
parseOptions = Options <$> parseOwner <*> parseAddress

main :: IO ()
main = doCreateAddress =<< execParser opts
  where
    opts = info (parseOptions <**> helper)
      ( fullDesc
     <> progDesc "Create Cardano address"
     <> header "create-address - a simple address creator" )

doCreateAddress :: Options -> IO ()
doCreateAddress (Options owner address) = do
  loadFile defaultConfig
  addressesPath <- getEnv "ADDRESSES_PATH"
  policiesFolder <- getEnv "POLICIES_FOLDER"
  let cOwner = capitalized owner

  network <- getEnv "NETWORK"
  snetworkMagic <- getEnv "NETWORK_MAGIC"
  let networkMagic = read snetworkMagic :: Int
  let bNetwork = BlockchainNetwork { network = "--" ++ network, networkMagic = networkMagic, networkEra = "", networkEnv = "" }
  
  Control.Monad.when (paymentKey address) $ do 
    rc <- createKeypair Payment addressesPath cOwner
    putStrLn $ "Creating payment keypair for " ++ cOwner

  Control.Monad.when (stakeKey address) $ do 
    rc <- createKeypair Stake addressesPath cOwner
    putStrLn $ "Creating stake keypair for " ++ cOwner

  Control.Monad.when (payment address) $ do 
    ownerAddress <- createAddress bNetwork Payment addressesPath cOwner
    putStrLn $ "Creating payment address for " ++ cOwner ++ "\n"

  Control.Monad.when (stake address) $ do 
    ownerAddress <- createAddress bNetwork Stake addressesPath cOwner
    putStrLn $ "Creating stake address for " ++ cOwner ++ "\n"
  
