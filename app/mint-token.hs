import           Address              (Address (..), AddressType (Payment),
                                       getAddressFromFile, getAddressFile, getSKeyFile)
import           Baseutils            (capitalized, formatNumber)
import           Configuration.Dotenv (defaultConfig, loadFile)
import           Control.Monad        (unless, void, when)
import           Data.Maybe           (fromJust, fromMaybe, isJust, isNothing)
import           Data.Semigroup       ((<>))
import           Network              (BlockchainNetwork (..))
import           Options.Applicative
import           Policy               (Policy (..), buildPolicyName,
                                       getPolicies, getPoliciesPath, getPolicy)
import           Protocol             (saveProtocolParameters)
import           System.Directory     (doesFileExist)
import           System.Environment   (getEnv, lookupEnv)
import           TokenUtils           (Token (..), calculateTokensBalance,
                                       getTokenId, getTokenPath,
                                       readTokensFromFile, recordTokens)
import           Transaction          (FileType (..),
                                       Utxo (Utxo, nbUtxos, raw, tokens, utxos),
                                       buildMintTransaction, calculateMintFees,
                                       getTransactionFile, getUtxoFromWallet,
                                       signMintTransaction, submitTransaction)
import Wallet
    ( Owner (..) )

-- parsing options
data TokenOptions = TokenAmount TokenAmountOption
                  | TokensFile FilePath
                  deriving (Eq, Show)

data TokenAmountOption = TokenAmountOption
  { token      :: String
  , amount     :: Int
  , policy      :: String
  } deriving (Eq, Show)

data MintOptions = MintOptions
  { ownerName       :: String
  , metadata    :: Maybe String
  , createToken :: Bool
  } deriving (Show)

data DstTypeAddress = DstName String
                    | DstAddress String
                    | DstFile FilePath
                    deriving (Eq, Show)

data Options = Options MintOptions DstTypeAddress TokenOptions

parseMint :: Parser MintOptions
parseMint = MintOptions
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
  <*> switch
  ( long "create-token"
  <> short 'c'
  <> help "create token if it does not exist")

parseTokenOptions :: Parser TokenOptions
parseTokenOptions =  parseTokenAmount <|> parseTokensFile

parseTokenAmount :: Parser TokenOptions

parseTokenAmount = TokenAmount <$> parseTokenAmountOption

parseTokenAmountOption :: Parser TokenAmountOption
parseTokenAmountOption = TokenAmountOption <$> strOption ( long "token" <> short 't' <> metavar "TOKEN" <> help "token" )
                            <*>  option auto ( long "amount" <> short 'n' <> metavar "AMOUNT" <> help "tokens amount" <> value 0 )
                            <*> strOption ( long "policy" <> short 'p' <> metavar "POLICY" <> help "token policy" )

parseTokensFile :: Parser TokenOptions
parseTokensFile = TokensFile <$> strOption ( long "tokens-file" <> short 'f' <> metavar "TOKENS" <> help "json tokens file" )

parseDstName :: Parser DstTypeAddress
parseDstName = DstName <$> strOption ( long "destination" <> short 'd' <> metavar "DESTINATION NAME" <> help "name of destination address owner" )

parseDstAddress :: Parser DstTypeAddress
parseDstAddress = DstAddress <$> strOption ( long "address" <> short 'a' <> metavar "DESTINATION ADDRESS" <> help "destination address" )

parseDstFile :: Parser DstTypeAddress
parseDstFile = DstFile <$> strOption ( long "to-address" <> metavar "DESTINATION FILE" <> help "destination address file" )

parseDstTypeAddress :: Parser DstTypeAddress
parseDstTypeAddress = parseDstName <|> parseDstAddress <|> parseDstFile

parseOptions :: Parser Options
parseOptions = Options <$> parseMint <*> parseDstTypeAddress <*> parseTokenOptions

main :: IO ()
main = mintToken =<< execParser opts
  where
    opts = info (parseOptions <**> helper)
      ( fullDesc
     <> progDesc "Mint amount Token for address with signing key."
     <> header "mint-token - a simple minting token tool" )

-- mint token
mintToken :: Options -> IO ()
mintToken (Options mintOptions dstTypeAddress tokenOptions) = do
    loadFile defaultConfig
    addressesPath <- getEnv "ADDRESSES_PATH"
    policiesFolder <- getEnv "POLICIES_FOLDER"
    networkSocket <- getEnv "CARDANO_NODE_SOCKET_PATH"
    network <- getEnv "NETWORK"
    sNetworkMagic <- getEnv "NETWORK_MAGIC"
    let networkMagic = read sNetworkMagic :: Int
    networkEra <- lookupEnv "NETWORK_ERA"
    -- mint owner, policy and token
    let owner = Owner (capitalized $ ownerName mintOptions)
        mTokenMetadata = metadata mintOptions
        doCreateToken = createToken mintOptions
        policiesPath = getPoliciesPath addressesPath owner policiesFolder

    -- source address and signing key
    mSrcAddress <- getSrcAddress owner addressesPath
    let sKeyFile = getSKeyFile addressesPath Payment owner

    -- destination address
    mDstAddress <- getDstAddress dstTypeAddress addressesPath

    -- tokens file
    tokenList <- getTokenOption tokenOptions

    Control.Monad.when (isJust mSrcAddress && isJust mDstAddress) $ do
        -- print tokenList
        let bNetwork = BlockchainNetwork { network = "--" ++ network, networkMagic = networkMagic, networkEra = networkEra, networkEnv = networkSocket }
        doMint bNetwork owner mSrcAddress sKeyFile mDstAddress policiesPath tokenList mTokenMetadata

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
  let payee = Owner (capitalized dstName)
  maddress <- getAddressFromFile $ getAddressFile addressesPath Payment payee
  case maddress of
    Just address -> do
      let dstAddress = fromJust maddress
      putStrLn $ "Destination address : " ++ getAddress dstAddress
    _ -> putStrLn $ "No " ++ show Payment ++ " address for " ++ getOwner payee
  return maddress
getDstAddress (DstAddress dstAddress) addressesPath = do
  putStrLn $ "Destination address : " ++ dstAddress
  return (Just $ Address dstAddress)
getDstAddress (DstFile dstFile) addressesPath = do
  maddress <- getAddressFromFile dstFile
  case maddress of
    Just address -> do
      let dstAddress = fromJust maddress
      putStrLn $ "Destination address : " ++ getAddress dstAddress
    _ -> putStrLn $ "No " ++ show Payment ++ " address for " ++ dstFile
  return maddress

-- get tokens depending on token option
getTokenOption :: TokenOptions -> IO [Token]
getTokenOption (TokenAmount tokenAmountOption) =
    return [Token { tokenName = token tokenAmountOption, tokenAmount = amount tokenAmountOption,
            tokenId = getTokenId (policy tokenAmountOption) (token tokenAmountOption), tokenPolicyName = policy tokenAmountOption }]
getTokenOption (TokensFile tokensFile) = readTokensFromFile (Just tokensFile)

-- mint amount of token from owner for destination address on given network
doMint :: BlockchainNetwork -> Owner -> Maybe Address -> FilePath -> Maybe Address -> String -> [Token] -> Maybe String -> IO ()
doMint bNetwork ownerName mSrcAddress sKeyFile mDstAddress policiesPath tokenList mTokenMetadata
  | null tokenList || isNothing mSrcAddress = putStrLn "Missing valid address or token"
  | otherwise = do
    -- 2. Extract protocol parameters (needed for fee calculations)
    let protocolParametersFile = "/tmp/protocolParams.json"
    saveProtocolParameters bNetwork protocolParametersFile

    -- 3. Get UTXOs from our wallet
    utxo <- getUtxoFromWallet bNetwork (fromJust mSrcAddress)

    -- 5. Calculate fees for the transaction
    policies <- getPolicies policiesPath [tokenPolicyName token | token <- tokenList ]
    minFee <- calculateMintFees bNetwork (fromJust mSrcAddress) policies tokenList mTokenMetadata utxo protocolParametersFile

    if isNothing minFee then
      putStrLn "Unable to calculate min fee."
    else do
      -- 6. Build actual transaction including correct fees
      unless (null tokenList) $ do
        let mTokenName = Just (tokenName (head tokenList))
        let okFeeFile = getTransactionFile mTokenName OkFee
        rc <- buildMintTransaction bNetwork (fromJust mSrcAddress) (fromJust mDstAddress) tokenList mTokenMetadata utxo (fromJust minFee) okFeeFile
        unless rc $ print "Failed to build transaction"

        -- 7. Sign the transaction
        let signFile = getTransactionFile mTokenName Sign
        signMintTransaction bNetwork sKeyFile policies okFeeFile signFile

        -- 8. Submit the transaction to the blockchain
        rc <- submitTransaction bNetwork signFile
        when rc $ do
          unless (null tokenList) $ do
            recordTokens policiesPath tokenList
            putStrLn $ "Tokens minted with " ++ formatNumber (fromJust minFee) ++ " lovelaces fee"
            putStrLn $ "Tokens : " ++ unwords [tokenName token | token <- tokenList]
