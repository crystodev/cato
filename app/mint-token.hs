import System.Environment ( getEnv )
import Configuration.Dotenv (loadFile, defaultConfig)
import Options.Applicative
import Data.Semigroup ((<>))
import Control.Monad (void, when, unless)
import Data.Maybe ( isJust, fromJust )
import Baseutils ( capitalized )
import Tokutils ( Address, AddressType(Payment), BlockchainNetwork(BlockchainNetwork, network, networkMagic, networkEra, networkEnv), 
  calculateTokensBalance, createPolicy, getAddress, getAddressFile, getPolicy, getPolicyPath, Policy(Policy, policyId ), getSkeyFile, 
  recordToken, saveProtocolParameters )
import Transaction ( buildMintTransaction, calculateMintFees, getTransactionFile, FileType(..), getUtxoFromWallet, signMintTransaction,
  submitTransaction, Utxo(Utxo, raw, utxos, nbUtxos, tokens) )

type Owner = String
-- parsing options
data MintOptions = MintOptions 
  { owner :: String
  , policy :: String 
  , token :: String 
  , amount :: Int 
  } deriving (Show)
data DstTypeAddress = DstName String 
                    | DstAddress String
                    | DstFile FilePath 
                    deriving (Eq, Show)

data Options = Options MintOptions DstTypeAddress

parseMint :: Parser MintOptions
parseMint = MintOptions
  <$> strOption
  ( long "owner"
  <> short 'o'
  <> metavar "OWNER"
  <> help "address owner name" )
  <*> strOption
  ( long "policy"
  <> short 'p'
  <> metavar "POLICY"
  <> help "token policy" )
  <*> strOption
  ( long "token"
  <> short 't'
  <> metavar "TOKEN"
  <> help "token" )
  <*> option auto
  ( long "amount"
  <> short 'm'
  <> metavar "AMOUNT"
  <> help "tokens amount" )

parseDstName :: Parser DstTypeAddress
parseDstName = DstName <$> strOption ( long "destination" <> short 'd' <> metavar "DESTINATION NAME" <> help "name of destination address owner" )

parseDstAddress :: Parser DstTypeAddress
parseDstAddress = DstAddress <$> strOption ( long "address" <> short 'a' <> metavar "DESTINATION ADDRESS" <> help "destination address" )

parseDstFile :: Parser DstTypeAddress
parseDstFile = DstFile <$> strOption ( long "to-address" <> metavar "DESTINATION FILE" <> help "destination address file" )

parseDstTypeAddress :: Parser DstTypeAddress
parseDstTypeAddress = parseDstName <|> parseDstAddress <|> parseDstFile

parseOptions :: Parser Options
parseOptions = Options <$> parseMint <*> parseDstTypeAddress

main :: IO ()
main = mintToken =<< execParser opts
  where
    opts = info (parseOptions <**> helper)
      ( fullDesc
     <> progDesc "Mint amount Token for address with signing key."
     <> header "mint-token - a simple minting token tool" )

-- mint token
mintToken :: Options -> IO ()
mintToken (Options mintOptions dstTypeAddress) = do
  loadFile defaultConfig
  addressesPath <- getEnv "ADDRESSES_PATH"
  policiesFolder <- getEnv "POLICIES_FOLDER"
  networkSocket <- getEnv "CARDANO_NODE_SOCKET_PATH"
  network <- getEnv "NETWORK"
  snetworkMagic <- getEnv "NETWORK_MAGIC"
  let networkMagic = read snetworkMagic :: Int
  networkEra <- getEnv "NETWORK_ERA"
  -- mint owner, policy and token
  let ownerName = capitalized $ owner mintOptions
      policyName = policy mintOptions
      tokenName = token mintOptions
      tokenAmount = amount mintOptions
      policiesPath = getPolicyPath addressesPath ownerName policyName policiesFolder
      
  -- source address and signing key
  msrcAddress <- getSrcAddress ownerName addressesPath
  let skeyFile = getSkeyFile addressesPath Payment ownerName

  -- destination address
  mdstAddress <- getDstAddress dstTypeAddress addressesPath

  Control.Monad.when (isJust msrcAddress && isJust mdstAddress) $ do
    let bNetwork = BlockchainNetwork { network = "--" ++ network, networkMagic = networkMagic, networkEra = "--" ++ networkEra, networkEnv = networkSocket }
    doMint bNetwork ownerName msrcAddress skeyFile mdstAddress policyName policiesPath (Just tokenName) tokenAmount
  putStrLn ""

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

-- build policy name
-- if no policy name specified, search policy with token name
buildPolicyName :: String -> Maybe String -> String
buildPolicyName "" Nothing = "noName"
buildPolicyName "" (Just tokenName) = tokenName
buildPolicyName policyName _ = policyName

-- mint amount of token from owner for destination address on given network
doMint :: BlockchainNetwork -> Owner -> Maybe Address -> FilePath -> Maybe Address -> String -> String -> Maybe String -> Int -> IO ()
doMint bNetwork ownerName msrcAddress skeyFile mdstAddress policyName policiesPath mtokenName tokenAmount = do
  let protocolParametersFile = "/tmp/protparams.json"
      -- 1. Create a policy for our token
      polName = buildPolicyName policyName mtokenName
  
  policy <- createPolicy polName policiesPath
  -- let policy = getPolicy policyName policiesPath
  when (isJust policy && tokenAmount /= 0 && isJust msrcAddress) $ do
    -- 2. Extract protocol parameters (needed for fee calculations)
    saveProtocolParameters bNetwork protocolParametersFile

    -- 3. Get UTXOs from our wallet
    utxo <- getUtxoFromWallet bNetwork (fromJust msrcAddress)

    -- 4. Calculate tokens balance
    let balances = calculateTokensBalance(tokens utxo)
    -- print balances

    -- 5. Calculate fees for the transaction
    minFee <- calculateMintFees bNetwork (fromJust msrcAddress) mtokenName tokenAmount (policyId (fromJust policy)) utxo protocolParametersFile
    -- print (fromJust minFee)

    when (isJust minFee) $ do
      -- 6. Build actual transaction including correct fees
      let okFeeFile = getTransactionFile mtokenName OkFee
      rc <- buildMintTransaction bNetwork (fromJust msrcAddress) (fromJust mdstAddress) mtokenName tokenAmount (policyId (fromJust policy)) utxo (fromJust minFee) okFeeFile
      unless rc $ print "Failed to build transaction"
      
      -- 7. Sign the transaction
      let signFile = getTransactionFile mtokenName Sign
      signMintTransaction bNetwork skeyFile (fromJust policy) okFeeFile signFile

      -- 8. Submit the transaction to the blockchain
      rc <- submitTransaction bNetwork signFile
      when rc $ do
        when (isJust mtokenName) $ do
          recordToken (fromJust policy) (fromJust mtokenName)
      -- print signFile
