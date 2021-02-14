import           Address              (Address (..), AddressType (Payment),
                                       getAddressFromFile, getAddressFile, getSKeyFile)
import           Baseutils            (capitalized)
import           Configuration.Dotenv (defaultConfig, loadFile)
import           Control.Monad        (unless, void, when)
import           Data.Maybe           (fromJust, fromMaybe, isJust, isNothing)
import           Data.Semigroup       ((<>))
import           Network              (BlockchainNetwork (..))
import           Options.Applicative
import           Protocol             (saveProtocolParameters)
import           System.Environment   (getEnv, lookupEnv)
import           TokenUtils           (calculateTokensBalance)
import           Transaction          (FileType (..),
                                       Utxo (Utxo, nbUtxos, raw, tokens, utxos),
                                       buildSendTransaction, calculateSendFees,
                                       getTokenIdFromName, getTransactionFile,
                                       getUtxoFromWallet, signSendTransaction,
                                       submitTransaction)

import Wallet
    ( Owner (..) )

-- parsing options
data SendOptions = SendOptions
  { owner    :: String
  , metadata :: Maybe String
  , ada      :: Int
  } deriving (Show)
data DstTypeAddress = DstName String
                    | DstAddress String
                    | DstFile FilePath
                    deriving (Eq, Show)

data Options = Options SendOptions DstTypeAddress

parseSend :: Parser SendOptions
parseSend = SendOptions
  <$> strOption
    ( long "owner"
    <> short 'o'
    <> metavar "OWNER"
    <> help "address owner name" )
  <*> optional ( strOption
  ( long "json"
  <> short 'j'
  <> metavar "METADATA"
  <> help "token metadata"))
  <*> option auto
    ( long "ada"
    <> metavar "ADA AMOUNT"
    <> help "ada amount"
    <> value 0)

parseDstName :: Parser DstTypeAddress
parseDstName = DstName <$> strOption ( long "destination" <> short 'd' <> metavar "DESTINATION NAME" <> help "name of destination address owner" )

parseDstAddress :: Parser DstTypeAddress
parseDstAddress = DstAddress <$> strOption ( long "address" <> short 'a' <> metavar "DESTINATION ADDRESS" <> help "destination address" )

parseDstFile :: Parser DstTypeAddress
parseDstFile = DstFile <$> strOption ( long "to-address" <> metavar "DESTINATION FILE" <> help "destination address file" )

parseDstTypeAddress :: Parser DstTypeAddress
parseDstTypeAddress = parseDstName <|> parseDstAddress <|> parseDstFile

parseOptions :: Parser Options
parseOptions = Options <$> parseSend <*> parseDstTypeAddress

main :: IO ()
main = sendToken =<< execParser opts
  where
    opts = info (parseOptions <**> helper)
      ( fullDesc
     <> progDesc "Send amount of Ada from owner to destination."
     <> header "send-ada - a simple send ada tool" )

-- send token
sendToken :: Options -> IO ()
sendToken (Options sendOptions dstTypeAddress) = do
  loadFile defaultConfig
  addressesPath <- getEnv "ADDRESSES_PATH"
  policiesFolder <- getEnv "POLICIES_FOLDER"
  networkSocket <- getEnv "CARDANO_NODE_SOCKET_PATH"
  network <- getEnv "NETWORK"
  sNetworkMagic <- getEnv "NETWORK_MAGIC"
  let networkMagic = read sNetworkMagic :: Int
  networkEra <- lookupEnv "NETWORK_ERA"

  let ownerName = Owner (capitalized $ owner sendOptions)
      adaAmount = ada sendOptions
      mTokenMetadata = metadata sendOptions

  -- source address and signing key
  mSrcAddress <- getSrcAddress ownerName addressesPath
  let sKeyFile = getSKeyFile addressesPath Payment ownerName

  -- destination address
  mDstAddress <- getDstAddress dstTypeAddress addressesPath

  Control.Monad.when (isJust mSrcAddress && isJust mDstAddress) $ do
    let bNetwork = BlockchainNetwork { network = "--" ++ network, networkMagic = networkMagic, networkEra = networkEra, networkEnv = networkSocket }
    rc <- doSend bNetwork ownerName mSrcAddress sKeyFile mDstAddress adaAmount mTokenMetadata
    unless rc $ putStrLn "Nothing sent"


-- get srcAddress from owner
getSrcAddress :: Owner -> FilePath -> IO (Maybe Address)
getSrcAddress owner addressesPath = do
  maddress <- getAddressFromFile $ getAddressFile addressesPath Payment owner
  case maddress of
    Just address -> do
      let srcAddress = fromJust maddress
      putStrLn $ "Source address : " ++ getAddress srcAddress
    _ -> putStrLn $ "No " ++ show Payment ++ " address for " ++ getOwner owner
  return maddress

-- get dstAddress depending on type address provided
getDstAddress :: DstTypeAddress -> FilePath -> IO (Maybe Address)
getDstAddress (DstName dstName) addressesPath = do
  maddress <- getAddressFromFile $ getAddressFile addressesPath Payment (Owner $ capitalized dstName)
  case maddress of
    Just address -> do
      let dstAddress = fromJust maddress
      putStrLn $ "Destination address : " ++ getAddress dstAddress
    _ -> putStrLn $ "No " ++ show Payment ++ " address for " ++ dstName
  return maddress
getDstAddress (DstAddress dstAddress) addressesPath = do
  putStrLn $ "Destination address : " ++ show dstAddress
  return (Just $ Address dstAddress)
getDstAddress (DstFile dstFile) addressesPath = do
  maddress <- getAddressFromFile dstFile
  case maddress of
    Just address -> do
      let dstAddress = fromJust maddress
      putStrLn $ "Destination address : " ++ getAddress dstAddress
    _ -> putStrLn $ "No " ++ show Payment ++ " address for " ++ dstFile
  return maddress


-- send amount of ADA from owner to destination address on given network
doSend :: BlockchainNetwork -> Owner -> Maybe Address -> FilePath -> Maybe Address -> Int -> Maybe String -> IO Bool
doSend bNetwork ownerName mSrcAddress sKeyFile mDstAddress adaAmount mTokenMetadata
  | isNothing mSrcAddress = do
      putStrLn $  "No address found for " ++ getOwner ownerName
      return False
  | otherwise = do
    let protocolParametersFile = "/tmp/protocolParams.json"

    -- 1. Extract protocol parameters (needed for fee calculations)
    saveProtocolParameters bNetwork protocolParametersFile

    -- 2. Get UTXOs from our wallet
    utxo <- getUtxoFromWallet bNetwork (fromJust mSrcAddress)

    -- 4. Calculate tokens balance
    let balances = calculateTokensBalance(tokens utxo)

    -- 5. Calculate fees for the transaction
    minFee <- calculateSendFees bNetwork (fromJust mSrcAddress) (fromJust mDstAddress) adaAmount [] mTokenMetadata utxo protocolParametersFile
    -- print (fromJust minFee)

    when (isJust minFee) $ do
      -- 6. Build actual transaction including correct fees
      let okFeeFile = getTransactionFile Nothing OkFee
      rc <- buildSendTransaction bNetwork (fromJust mSrcAddress) (fromJust mDstAddress) adaAmount [] mTokenMetadata utxo (fromJust minFee) okFeeFile
      unless rc $ do
        putStrLn "Failed to build transaction"

      -- 7. Sign the transaction
      let signFile = getTransactionFile Nothing Sign
      signSendTransaction bNetwork sKeyFile okFeeFile signFile

      -- 8. Submit the transaction to the blockchain
      rc <- submitTransaction bNetwork signFile
      putStrLn ""
      -- putStrLn signFile
    return True
