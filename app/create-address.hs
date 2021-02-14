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

newtype OwnerName = OwnerName { getOwnerName :: String} deriving (Eq, Show)
data Address = Address {
  payment    :: Bool
, stake      :: Bool
, paymentKey :: Bool
, stakeKey   :: Bool
} deriving Show

data Options = Options OwnerName Address

parseOwner :: Parser OwnerName
parseOwner = OwnerName <$> argument str (metavar "OWNER")

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
doCreateAddress (Options ownerName address) = do
  loadFile defaultConfig
  addressesPath <- getEnv "ADDRESSES_PATH"
  policiesFolder <- getEnv "POLICIES_FOLDER"
  let owner = Owner (capitalized $ getOwnerName ownerName)

  network <- getEnv "NETWORK"
  sNetworkMagic <- getEnv "NETWORK_MAGIC"
  let networkMagic = read sNetworkMagic :: Int
  let bNetwork = BlockchainNetwork { network = "--" ++ network, networkMagic = networkMagic, networkEra = Nothing, networkEnv = "" }

  Control.Monad.when (paymentKey address) $ do
    rc <- createKeyPair Payment addressesPath owner
    putStrLn $ "Creating payment key pair for " ++ getOwner owner

  Control.Monad.when (stakeKey address) $ do
    rc <- createKeyPair Stake addressesPath owner
    putStrLn $ "Creating stake key pair for " ++ getOwner owner

  Control.Monad.when (payment address) $ do
    ownerAddress <- createAddress bNetwork Payment addressesPath owner
    putStrLn $ "Creating payment address for " ++ getOwner owner ++ "\n"

  Control.Monad.when (stake address) $ do
    ownerAddress <- createAddress bNetwork Stake addressesPath owner
    putStrLn $ "Creating stake address for " ++ getOwner owner ++ "\n"

