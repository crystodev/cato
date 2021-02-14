import Address
    ( AddressType (Payment, Stake), createKeyPair )
import Baseutils
    ( capitalized )
import Configuration.Dotenv
    ( defaultConfig, loadFile )
import Control.Monad
    ( void, when )
import Data.Maybe
    ( fromJust, isJust )
import Data.Semigroup
    ( (<>) )
import Network
    ( BlockchainNetwork (..) )
import Options.Applicative
import System.Environment
    ( getEnv, lookupEnv )
import Transaction
    ( createAddress )
import Wallet
    ( Owner (..) )

data Address = Address {
  payment    :: Bool
, stake      :: Bool
, paymentKey :: Bool
, stakeKey   :: Bool
} deriving Show

type OwnerName = String

data Options = Options OwnerName Address

parseOwner :: Parser OwnerName
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
  let cOwner = Owner (capitalized $ show owner)

  network <- getEnv "NETWORK"
  sNetworkMagic <- getEnv "NETWORK_MAGIC"
  let networkMagic = read sNetworkMagic :: Int
  let bNetwork = BlockchainNetwork { network = "--" ++ network, networkMagic = networkMagic, networkEra = Nothing, networkEnv = "" }

  Control.Monad.when (paymentKey address) $ do
    rc <- createKeyPair Payment addressesPath cOwner
    putStrLn $ "Creating payment key pair for " ++ show cOwner

  Control.Monad.when (stakeKey address) $ do
    rc <- createKeyPair Stake addressesPath cOwner
    putStrLn $ "Creating stake key pair for " ++ show cOwner

  Control.Monad.when (payment address) $ do
    ownerAddress <- createAddress bNetwork Payment addressesPath cOwner
    putStrLn $ "Creating payment address for " ++ show cOwner ++ "\n"

  Control.Monad.when (stake address) $ do
    ownerAddress <- createAddress bNetwork Stake addressesPath cOwner
    putStrLn $ "Creating stake address for " ++ show cOwner ++ "\n"

