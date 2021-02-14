import Address
    ( Address (..), AddressType (Payment, Stake), getAddress, getAddressFile )
import Baseutils
    ( capitalized )
import Configuration.Dotenv
    ( defaultConfig, loadFile )
import Control.Monad
    ( unless, void, when )
import Data.List
    ( unlines )
import Data.List.Split
    ( splitOn )
import Data.Maybe
    ( fromJust, isNothing )
import Data.Semigroup
    ( (<>) )
import Data.Text.Format.Numbers
import Network
    ( BlockchainNetwork (..) )
import Options.Applicative
import System.Environment
    ( getEnv, lookupEnv )
import Text.Printf
    ( printf )
import TokenUtils
    ( calculateTokensBalance )
import Transaction
    ( Utxo (Utxo, nbUtxos, raw, tokens, utxos), getUtxoFromWallet )
import Wallet
    ( Owner (..) )

-- parsing options
data InfoAddress = InfoAddress {
  payment :: Bool
, stake :: Bool
, balance :: Bool
, utxo :: Bool
, compact :: Bool
} deriving Show

type OwnerName = String

data Options = Options OwnerName InfoAddress

parseOwner :: Parser OwnerName
parseOwner = argument str (metavar "OWNER")

parseInfoAddress :: Parser InfoAddress
parseInfoAddress = InfoAddress
          <$> switch ( long "payment" <> short 'p' <> help "payment address" )
          <*> switch ( long "stake" <> short 's' <> help "stake address" )
          <*> switch ( long "balance" <> short 'b' <> help "display balances" )
          <*> switch ( long "utxo" <> short 'u' <> help "display utxo" )
          <*> switch ( long "compact" <> short 'c' <> help "displays in compact mode" )

parseOptions :: Parser Options
parseOptions = Options <$> parseOwner <*> parseInfoAddress

-- display balance from address
printBalance :: BlockchainNetwork -> Address -> IO ()
printBalance bNetwork address = do
  _utxo <- getUtxoFromWallet bNetwork address
  let tokens' = calculateTokensBalance $ tokens _utxo
  printf "%32s%52s\n" "Token" "Amount"
  putStrLn "----------------------------------------------------------------------------------------"
  let x = fmap printTokenBalance tokens'
  putStrLn $ unlines x

-- format balance for token
printTokenBalance :: (String, Int) -> String
printTokenBalance (token, balance) = do
  printf "%-77s%11s" token ( prettyI (Just ' ') balance)

-- display utxo from address
printUtxo :: Bool -> BlockchainNetwork -> Address -> IO ()
printUtxo compactMode bNetwork address = do
  _utxo <- getUtxoFromWallet bNetwork address
  if compactMode then
    putStrLn $ unwords [ if '.' `elem` word then drop 50 word else word | word <- splitOn " " (raw _utxo)]
  else
    putStrLn $ raw _utxo

main :: IO ()
main = getInfoAddress =<< execParser opts
  where
    opts = info (parseOptions <**> helper)
      ( fullDesc
     <> progDesc "Get utxo or balance for address of user Name"
     <> header "address-info - a simple address info tool" )

getInfoAddress :: Options -> IO ()
getInfoAddress (Options ownerName infoAddress) = do
  loadFile defaultConfig
  addressesPath <- getEnv "ADDRESSES_PATH"
  policiesFolder <- getEnv "POLICIES_FOLDER"
  networkSocket <- getEnv "CARDANO_NODE_SOCKET_PATH"

  let owner = Owner (capitalized ownerName)

  network <- getEnv "NETWORK"
  sNetworkMagic <- getEnv "NETWORK_MAGIC"
  let networkMagic = read sNetworkMagic :: Int
  networkEra <- lookupEnv "NETWORK_ERA"

  let bNetwork = BlockchainNetwork { network = "--" ++ network, networkMagic = networkMagic, networkEra = networkEra, networkEnv = networkSocket }

  let addressType = if stake infoAddress then Stake else Payment
  maddress <- getAddress $ getAddressFile addressesPath addressType owner

  case maddress of
    Just address -> do
      let compactMode = compact infoAddress
      let address = fromJust maddress
      putStrLn $ "Address : " ++ show address

      Control.Monad.when (balance infoAddress) $ do
        printBalance bNetwork address

      Control.Monad.when (utxo infoAddress) $ do
        printUtxo compactMode bNetwork address
    _ -> putStrLn $ "No " ++ show addressType ++ " address for " ++ show owner
