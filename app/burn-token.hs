{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
import Address
    ( Address, AddressType (Payment), getAddress, getAddressFile, getSKeyFile )
import Baseutils
    ( capitalized, formatNumber )
import Configuration.Dotenv
    ( defaultConfig, loadFile )
import Control.Monad
    ( unless, void, when )
import Data.Maybe
    ( fromJust, isJust, isNothing )
import Data.Semigroup
    ( (<>) )
import Network
    ( BlockchainNetwork (..) )
import Options.Applicative
import Policy
    ( Policy (..), buildPolicyName, getPolicies, getPoliciesPath, getPolicy )
import Protocol
    ( saveProtocolParameters )
import System.Environment
    ( getEnv, lookupEnv )
import TokenUtils
    ( Token (..), calculateTokensBalance, getTokenId )
import Transaction
    ( FileType (..)
    , Utxo (Utxo, nbUtxos, raw, tokens, utxos)
    , buildBurnTransaction
    , calculateBurnFees
    , getTransactionFile
    , getUtxoFromWallet
    , signBurnTransaction
    , submitTransaction
    )
import Wallet
    ( Owner (..) )

-- parsing options
data BurnOptions = BurnOptions
  { ownerName  :: String
  , policy :: String
  , token  :: String
  , amount :: Int
  } deriving (Show)
data DstTypeAddress = DstName String
                    | DstAddress String
                    | DstFile FilePath
                    deriving (Eq, Show)

newtype Options = Options BurnOptions

parseBurn :: Parser BurnOptions
parseBurn = BurnOptions
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
parseOptions = Options <$> parseBurn

main :: IO ()
main = burnToken =<< execParser opts
  where
    opts = info (parseOptions <**> helper)
      ( fullDesc
     <> progDesc "Burn amount Token for address with signing key."
     <> header "burn-token - a simple burning token tool" )

-- burn token
burnToken :: Options -> IO ()
burnToken (Options burnOptions) = do
  loadFile defaultConfig
  addressesPath <- getEnv "ADDRESSES_PATH"
  policiesFolder <- getEnv "POLICIES_FOLDER"
  networkSocket <- getEnv "CARDANO_NODE_SOCKET_PATH"
  network <- getEnv "NETWORK"
  sNetworkMagic <- getEnv "NETWORK_MAGIC"
  let networkMagic = read sNetworkMagic :: Int
  networkEra <- lookupEnv "NETWORK_ERA"
  -- mint owner, policy and token
  let owner = Owner (capitalized $ ownerName burnOptions)
      policyName = policy burnOptions
      tokenName = token burnOptions
      tokenAmount = amount burnOptions
      policiesPath = getPoliciesPath addressesPath owner policiesFolder

  -- source address and signing key
  mSrcAddress <- getSrcAddress owner addressesPath
  let sKeyFile = getSKeyFile addressesPath Payment owner

  Control.Monad.when (isJust mSrcAddress) $ do
    let bNetwork = BlockchainNetwork { network = "--" ++ network, networkMagic = networkMagic, networkEra = networkEra, networkEnv = networkSocket }
    doBurn bNetwork owner mSrcAddress sKeyFile policyName policiesPath (Just tokenName) tokenAmount
  putStrLn ""

-- get srcAddress from owner
getSrcAddress :: Owner -> FilePath -> IO (Maybe Address)
getSrcAddress owner addressesPath = do
  maddress <- getAddress $ getAddressFile addressesPath Payment owner
  case maddress of
    Just address -> do
      let srcAddress = fromJust maddress
      putStrLn $ "Source address : " ++ show srcAddress
    _ -> putStrLn $ "No " ++ show Payment ++ " address for " ++ show owner
  return maddress

-- burn amount of token from owner for destination address on given network
doBurn :: BlockchainNetwork -> Owner -> Maybe Address -> FilePath -> String -> String -> Maybe String -> Int -> IO ()
doBurn bNetwork owner mSrcAddress sKeyFile policyName policiesPath mTokenName tokenAmount = do
  let protocolParametersFile = "/tmp/protocolParams.json"
      -- 1. get policy for our token
      polName = buildPolicyName policyName mTokenName

  policy <- getPolicy policiesPath polName
  when (isJust policy && tokenAmount /= 0 && isJust mSrcAddress) $ do
    -- 2. Extract protocol parameters (needed for fee calculations)
    saveProtocolParameters bNetwork protocolParametersFile

    -- 3. Get UTXOs from our wallet
    utxo <- getUtxoFromWallet bNetwork (fromJust mSrcAddress)

    -- 4. Calculate tokens balance
    let balances = calculateTokensBalance(tokens utxo)

    -- 5. Calculate fees for the transaction
    let polId = policyId (fromJust policy)
    let tokenList = [Token { tokenName = fromJust mTokenName, tokenAmount = tokenAmount,
                      tokenId = getTokenId polId (fromJust mTokenName),
                      tokenPolicyName = policyName} | isJust mTokenName ]

    policies <- getPolicies policiesPath [tokenPolicyName token | token <- tokenList ]
    minFee <- calculateBurnFees bNetwork (fromJust mSrcAddress) policies tokenList utxo balances protocolParametersFile

    when (isJust minFee) $ do
      -- 6. Build actual transaction including correct fees
      let okFeeFile = getTransactionFile mTokenName OkFee
      rc <- buildBurnTransaction bNetwork (fromJust mSrcAddress) tokenList utxo balances (fromJust minFee) okFeeFile
      unless rc $ print "Failed to build transaction"

      -- 7. Sign the transaction
      let signFile = getTransactionFile mTokenName Sign
      signBurnTransaction bNetwork sKeyFile policies okFeeFile signFile

      -- 8. Submit the transaction to the blockchain
      rc <- submitTransaction bNetwork signFile
      when rc $ do
        putStrLn $ "Tokens minted with " ++ formatNumber (fromJust minFee) ++ " lovelaces fee"
        putStrLn $ "Tokens : " ++ unwords [tokenName token | token <- tokenList]
