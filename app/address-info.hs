import System.Environment ( getEnv )
import Options.Applicative
import Data.Semigroup ((<>))
import Data.Maybe ( isNothing, fromJust )
import Baseutils ( capitalized )
import Control.Monad (void, when)
import Configuration.Dotenv (loadFile, defaultConfig)
import Tokutils ( Address, AddressType(Payment, Stake), BlockchainNetwork(BlockchainNetwork, network, networkMagic, networkEra, networkEnv), 
  calculateTokensBalance, getAddress, getAddressFile )
import Transaction ( getUtxoFromWallet, Utxo(Utxo, raw, utxos, nbUtxos, tokens) )
import Text.Printf ( printf )
import Data.List ( unlines )
import Data.Text.Format.Numbers

type Owner = String

-- parsing options
data InfoAddress = InfoAddress {
  payment :: Bool
, stake :: Bool
, balance :: Bool
, utxo :: Bool
} deriving Show

data Options = Options Owner InfoAddress

parseOwner :: Parser Owner
parseOwner = argument str (metavar "OWNER")

parseInfoAddress :: Parser InfoAddress
parseInfoAddress = InfoAddress
          <$> switch ( long "payment" <> short 'p' <> help "payment address" )
          <*> switch ( long "stake" <> short 's' <> help "stake address" )
          <*> switch ( long "balance" <> short 'b' <> help "display balances" )
          <*> switch ( long "utxo" <> short 'u' <> help "display utxo" )

parseOptions :: Parser Options
parseOptions = Options <$> parseOwner <*> parseInfoAddress

-- display balance from address
printBalance :: BlockchainNetwork -> String -> Address -> IO ()
printBalance bNetwork owner address = do
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
printUtxo :: BlockchainNetwork -> String -> Address -> IO ()
printUtxo bNetwork owner address = do
  _utxo <- getUtxoFromWallet bNetwork address
  putStrLn $ raw _utxo

main :: IO ()
main = getInfoAddress =<< execParser opts
  where
    opts = info (parseOptions <**> helper)
      ( fullDesc
     <> progDesc "Get utxo or balance for address of user Name"
     <> header "address-info - a simple address info tool" )

getInfoAddress :: Options -> IO ()
getInfoAddress (Options owner infoAddress) = do
  loadFile defaultConfig
  addressesPath <- getEnv "ADDRESSES_PATH"
  policiesFolder <- getEnv "POLICIES_FOLDER"
  networkSocket <- getEnv "CARDANO_NODE_SOCKET_PATH"

  let cOwner = capitalized owner

  network <- getEnv "NETWORK"
  snetworkMagic <- getEnv "NETWORK_MAGIC"
  let networkMagic = read snetworkMagic :: Int
  networkEra <- getEnv "NETWORK_ERA"

  let bNetwork = BlockchainNetwork { network = "--" ++ network, networkMagic = networkMagic, networkEra = "--" ++ networkEra, networkEnv = networkSocket }

  let addressType = if stake infoAddress then Stake else Payment
  maddress <- getAddress $ getAddressFile addressesPath addressType cOwner
  
  case maddress of
    Just address -> do
      let address = fromJust maddress
      putStrLn $ "Address : " ++ address

      Control.Monad.when (balance infoAddress) $ do 
        printBalance bNetwork cOwner address

      Control.Monad.when (utxo infoAddress) $ do 
        printUtxo bNetwork cOwner address
    _ -> putStrLn $ "No " ++ show addressType ++ " address for " ++ cOwner
