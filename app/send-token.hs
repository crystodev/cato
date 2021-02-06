import System.Environment ( getEnv, lookupEnv )
import Configuration.Dotenv (loadFile, defaultConfig)
import Options.Applicative
import Data.Semigroup ((<>))
import Control.Monad (void, when, unless)
import Data.Maybe ( isJust, isNothing, fromJust, fromMaybe )
import Baseutils ( capitalized )
import TokenUtils ( Address, AddressType(Payment), buildPolicyName, BlockchainNetwork(BlockchainNetwork, network, networkMagic, networkEra, networkEnv), 
  calculateTokensBalance, getAddress, getAddressFile, getPolicy, getPolicyIdFromTokenId, getPolicyPath, Policy(Policy, policyId), getSKeyFile, saveProtocolParameters )
import Transaction ( buildSendTransaction, calculateSendFees, getTransactionFile, FileType(..), getUtxoFromWallet, getTokenIdFromName, signSendTransaction,
  submitTransaction, Utxo(Utxo, raw, utxos, nbUtxos, tokens) )

type Owner = String
-- parsing options
data SendOptions = SendOptions 
  { owner :: String
  , policy :: String 
  , token :: String 
  , amount :: Int
  , ada :: Int
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
  <*> strOption
    ( long "policy"
    <> short 'p'
    <> metavar "POLICY"
    <> help "token policy"
    <> value "" )
  <*> strOption
    ( long "token"
    <> short 't'
    <> metavar "TOKEN"
    <> help "token" )
  <*> option auto
    ( long "amount"
    <> short 'm'
    <> metavar "TOKEN AMOUNT"
    <> help "token amount" )
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
     <> progDesc "Send amount of Ada and Token from owner to destination."
     <> header "send-token - a simple send token tool" )

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
  -- mint owner, policy and token
  let ownerName = capitalized $ owner sendOptions
      adaAmount = ada sendOptions
      policyName = policy sendOptions
      tokenName = token sendOptions
      tokenAmount = amount sendOptions
      policiesPath = getPolicyPath addressesPath ownerName policyName policiesFolder
      
  -- source address and signing key
  mSrcAddress <- getSrcAddress ownerName addressesPath
  let sKeyFile = getSKeyFile addressesPath Payment ownerName

  -- destination address
  mDstAddress <- getDstAddress dstTypeAddress addressesPath

  Control.Monad.when (isJust mSrcAddress && isJust mDstAddress) $ do
    let bNetwork = BlockchainNetwork { network = "--" ++ network, networkMagic = networkMagic, networkEra = networkEra, networkEnv = networkSocket }
    rc <- doSend bNetwork ownerName mSrcAddress sKeyFile mDstAddress adaAmount policyName policiesPath (Just tokenName) tokenAmount
    unless rc $ putStrLn "Nothing sent"


-- get srcAddress from owner
getSrcAddress :: Owner -> FilePath -> IO (Maybe Address)
getSrcAddress ownerName addressesPath = do
  maddress <- getAddress $ getAddressFile addressesPath Payment ownerName
  case maddress of
    Just address -> do
      let srcAddress = fromJust maddress
      putStrLn $ "Source address : " ++ srcAddress
    _ -> putStrLn $ "No " ++ show Payment ++ " address for " ++ ownerName
  return maddress

-- get dstAddress depending on type address provided
getDstAddress :: DstTypeAddress -> FilePath -> IO (Maybe Address)
getDstAddress (DstName dstName) addressesPath = do
  maddress <- getAddress $ getAddressFile addressesPath Payment (capitalized dstName)
  case maddress of
    Just address -> do
      let dstAddress = fromJust maddress
      putStrLn $ "Destination address : " ++ dstAddress
    _ -> putStrLn $ "No " ++ show Payment ++ " address for " ++ dstName
  return maddress
getDstAddress (DstAddress dstAddress) addressesPath = do
  putStrLn $ "Destination address : " ++ dstAddress
  return (Just dstAddress)
getDstAddress (DstFile dstFile) addressesPath = do
  maddress <- getAddress dstFile
  case maddress of
    Just address -> do
      let dstAddress = fromJust maddress
      putStrLn $ "Destination address : " ++ dstAddress
    _ -> putStrLn $ "No " ++ show Payment ++ " address for " ++ dstFile
  return maddress


-- send amount of token from owner for destination address on given network
doSend :: BlockchainNetwork -> Owner -> Maybe Address -> FilePath -> Maybe Address -> Int -> String -> String -> Maybe String -> Int -> IO Bool
doSend bNetwork ownerName mSrcAddress sKeyFile mDstAddress adaAmount policyName policiesPath mTokenName tokenAmount
  | isNothing mSrcAddress = do
      putStrLn $  "No address found for " ++ ownerName
      return False
  | isNothing mTokenName = do
      putStrLn "No token name provided"
      return False
  | tokenAmount == 0 = do
      putStrLn "Token amount must not be null"
      return False
  | otherwise = do
    let protocolParametersFile = "/tmp/protparams.json"
    
    -- 1. Extract protocol parameters (needed for fee calculations)
    saveProtocolParameters bNetwork protocolParametersFile

    -- 2. Get UTXOs from our wallet
    utxo <- getUtxoFromWallet bNetwork (fromJust mSrcAddress)

    -- 3. get policy for our token
    let mTokenId =  getTokenIdFromName (fromJust mTokenName) (tokens utxo)
    if isJust mTokenId then do
      let policyId = getPolicyIdFromTokenId (fromJust mTokenId)
      -- 4. Calculate tokens balance
      let balances = calculateTokensBalance(tokens utxo)

      -- 5. Calculate fees for the transaction
      minFee <- calculateSendFees bNetwork (fromJust mSrcAddress) (fromJust mDstAddress) adaAmount mTokenName tokenAmount policyId utxo protocolParametersFile
      -- print (fromJust minFee)

      when (isJust minFee) $ do
        -- 6. Build actual transaction including correct fees
        let okFeeFile = getTransactionFile mTokenName OkFee
        rc <- buildSendTransaction bNetwork (fromJust mSrcAddress) (fromJust mDstAddress) adaAmount mTokenName tokenAmount policyId utxo (fromJust minFee) okFeeFile
        unless rc $ do 
          putStrLn "Failed to build transaction"

        -- 7. Sign the transaction
        let signFile = getTransactionFile mTokenName Sign
        signSendTransaction bNetwork sKeyFile okFeeFile signFile

        -- 8. Submit the transaction to the blockchain
        rc <- submitTransaction bNetwork signFile
        putStrLn ""
        -- putStrLn signFile
      return True
    else do
      putStrLn $ "Address " ++ fromJust mSrcAddress ++ " has no token " ++ fromJust mTokenName
      return False