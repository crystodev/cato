import System.Environment ( getEnv, lookupEnv )
import Configuration.Dotenv (loadFile, defaultConfig)
import Options.Applicative
import Data.Semigroup ((<>))
import Control.Monad (void, when, unless)
import Data.Maybe ( isNothing, isJust, fromJust )
import Baseutils ( capitalized )
import Address ( Address, AddressType(Payment), getAddress, getAddressFile, getSKeyFile )
import Network ( BlockchainNetwork(..) )
import Policy ( buildPolicyName, getPolicy, getPoliciesPath, Policy(..) )
import Protocol ( saveProtocolParameters )
import TokenUtils ( calculateTokensBalance, getTokenId, Token(..) )
import Transaction ( buildBurnTransaction, calculateBurnFees, getTransactionFile, FileType(..), getUtxoFromWallet, signBurnTransaction,
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

newtype Options = Options MintOptions

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
  <> short 'n'
  <> metavar "AMOUNT"
  <> help "tokens amount" )

parseOptions :: Parser Options
parseOptions = Options <$> parseMint

main :: IO ()
main = burnToken =<< execParser opts
  where
    opts = info (parseOptions <**> helper)
      ( fullDesc
     <> progDesc "Burn amount Token for address with signing key."
     <> header "burn-token - a simple burning token tool" )

-- burn token
burnToken :: Options -> IO ()
burnToken (Options mintOptions) = do
  loadFile defaultConfig
  addressesPath <- getEnv "ADDRESSES_PATH"
  policiesFolder <- getEnv "POLICIES_FOLDER"
  networkSocket <- getEnv "CARDANO_NODE_SOCKET_PATH"
  network <- getEnv "NETWORK"
  sNetworkMagic <- getEnv "NETWORK_MAGIC"
  let networkMagic = read sNetworkMagic :: Int
  networkEra <- lookupEnv "NETWORK_ERA"
  -- mint owner, policy and token
  let ownerName = capitalized $ owner mintOptions
      policyName = policy mintOptions
      tokenName = token mintOptions
      tokenAmount = amount mintOptions
      policiesPath = getPoliciesPath addressesPath ownerName policiesFolder

  -- source address and signing key
  mSrcAddress <- getSrcAddress ownerName addressesPath
  let sKeyFile = getSKeyFile addressesPath Payment ownerName

  Control.Monad.when (isJust mSrcAddress) $ do
    let bNetwork = BlockchainNetwork { network = "--" ++ network, networkMagic = networkMagic, networkEra = networkEra, networkEnv = networkSocket }
    doBurn bNetwork ownerName mSrcAddress sKeyFile policyName policiesPath (Just tokenName) tokenAmount
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

-- burn amount of token from owner for destination address on given network
doBurn :: BlockchainNetwork -> Owner -> Maybe Address -> FilePath -> String -> String -> Maybe String -> Int -> IO ()
doBurn bNetwork ownerName mSrcAddress sKeyFile policyName policiesPath mTokenName tokenAmount = do
  let protocolParametersFile = "/tmp/protparams.json"
      -- 1. get policy for our token
      polName = buildPolicyName policyName mTokenName
  
  policy <- getPolicy polName policiesPath
  -- let policy = getPolicy policyName policiesPath
  when (isJust policy && tokenAmount /= 0 && isJust mSrcAddress) $ do
    -- 2. Extract protocol parameters (needed for fee calculations)
    saveProtocolParameters bNetwork protocolParametersFile

    -- 3. Get UTXOs from our wallet
    utxo <- getUtxoFromWallet bNetwork (fromJust mSrcAddress)

    -- 4. Calculate tokens balance
    let balances = calculateTokensBalance(tokens utxo)

    -- 5. Calculate fees for the transaction
    let polId = policyId (fromJust policy)
    -- let tokenList = ([Token { tokenName = fromJust mTokenName, tokenAmount = tokenAmount, tokenId = getTokenId polId (fromJust mTokenName)} | if isNothing mTokenName])
    let tokenList = if isNothing mTokenName then [] else ([Token { tokenName = fromJust mTokenName, tokenAmount = tokenAmount, tokenId = getTokenId polId (fromJust mTokenName)} ])
    minFee <- calculateBurnFees bNetwork (fromJust mSrcAddress) tokenList utxo balances protocolParametersFile
    -- print (fromJust minFee)

    when (isJust minFee) $ do
      -- 6. Build actual transaction including correct fees
      let okFeeFile = getTransactionFile mTokenName OkFee
      rc <- buildBurnTransaction bNetwork (fromJust mSrcAddress) tokenList utxo balances (fromJust minFee) okFeeFile
      unless rc $ print "Failed to build transaction"
      
      -- 7. Sign the transaction
      let signFile = getTransactionFile mTokenName Sign
      signBurnTransaction bNetwork sKeyFile (fromJust policy) okFeeFile signFile

      -- 8. Submit the transaction to the blockchain
      rc <- submitTransaction bNetwork signFile
      -- print signFile
      return ()
