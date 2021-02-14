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
  { token      :: Maybe String
  , amount     :: Int
  , tokensFile :: Maybe FilePath
  } deriving (Eq, Show)

data MintOptions = MintOptions
  { owner       :: String
  , policy      :: String
  , metadata    :: Maybe String
  , createToken :: Bool
 -- , token :: String
 -- , amount :: Int
  } deriving (Show)

data DstTypeAddress = DstName String
                    | DstAddress String
                    | DstFile FilePath
                    deriving (Eq, Show)

data Options = Options MintOptions DstTypeAddress TokenAmountOption

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
  <*> optional ( strOption
  ( long "json"
  <> short 'j'
  <> metavar "METADATA"
  <> help "token metadata"))
  <*> switch
  ( long "create-token"
  <> short 'c'
  <> help "create token if it does not exist")

{-
parseTokenOptions :: Parser TokenOptions
parseTokenOptions =  parseTokenAmount <|> parseTokensFile

parseTokenAmount :: Parser TokenOptions
parseTokenAmount =  parseTokenAmountOption
-}
parseTokenAmount :: Parser TokenAmountOption
parseTokenAmount = TokenAmountOption <$> optional (strOption ( long "token" <> short 't' <> metavar "TOKEN" <> help "token" ))
                               <*> option auto ( long "amount" <> short 'n' <> metavar "AMOUNT" <> help "tokens amount" <> value 0 )
                               <*> optional (strOption ( long "tokens-file" <> short 'f' <> metavar "TOKENS" <> help "json tokens file" ))

parseDstName :: Parser DstTypeAddress
parseDstName = DstName <$> strOption ( long "destination" <> short 'd' <> metavar "DESTINATION NAME" <> help "name of destination address owner" )

parseDstAddress :: Parser DstTypeAddress
parseDstAddress = DstAddress <$> strOption ( long "address" <> short 'a' <> metavar "DESTINATION ADDRESS" <> help "destination address" )

parseDstFile :: Parser DstTypeAddress
parseDstFile = DstFile <$> strOption ( long "to-address" <> metavar "DESTINATION FILE" <> help "destination address file" )

parseDstTypeAddress :: Parser DstTypeAddress
parseDstTypeAddress = parseDstName <|> parseDstAddress <|> parseDstFile

parseOptions :: Parser Options
parseOptions = Options <$> parseMint <*> parseDstTypeAddress <*> parseTokenAmount

main :: IO ()
main = mintToken =<< execParser opts
  where
    opts = info (parseOptions <**> helper)
      ( fullDesc
     <> progDesc "Mint amount Token for address with signing key."
     <> header "mint-token - a simple minting token tool" )

-- mint token
mintToken :: Options -> IO ()
mintToken (Options mintOptions dstTypeAddress tokenAmountOption) = do
  loadFile defaultConfig
  addressesPath <- getEnv "ADDRESSES_PATH"
  policiesFolder <- getEnv "POLICIES_FOLDER"
  networkSocket <- getEnv "CARDANO_NODE_SOCKET_PATH"
  network <- getEnv "NETWORK"
  sNetworkMagic <- getEnv "NETWORK_MAGIC"
  let networkMagic = read sNetworkMagic :: Int
  networkEra <- lookupEnv "NETWORK_ERA"
  -- mint owner, policy and token
  let ownerName = Owner (capitalized $ owner mintOptions)
      policyName = policy mintOptions
      mTokenMetadata = metadata mintOptions
      doCreateToken = createToken mintOptions
      mTokenName = token tokenAmountOption
      tokenAmount = amount tokenAmountOption
      mTokensFileName = tokensFile tokenAmountOption
      policiesPath = getPoliciesPath addressesPath ownerName policiesFolder

  -- source address and signing key
  mSrcAddress <- getSrcAddress ownerName addressesPath
  let sKeyFile = getSKeyFile addressesPath Payment ownerName

  -- destination address
  mDstAddress <- getDstAddress dstTypeAddress addressesPath

  -- tokens file
  tokenList' <- readTokensFromFile mTokensFileName

  mPolicy <- getPolicy policiesPath policyName
  if isNothing mTokenName then
    putStrLn "No valid token name found"
  else if isNothing mPolicy then
    putStrLn $ policyName ++ " : not a valid policy"
  else do
    let polId = policyId (fromJust mPolicy)
    let tokenList = [Token {tokenName = fromJust mTokenName, tokenAmount=tokenAmount,
                  tokenId = getTokenId polId (fromJust mTokenName),
                  tokenPolicyName = policyName } | isJust mTokenName ] ++ tokenList'
    let tokenName = fromJust mTokenName
    tokenExists <- doesFileExist $ getTokenPath policiesPath policyName tokenName
    if not tokenExists && not doCreateToken then
      putStrLn $ "Token " ++ tokenName ++ " does not exist ; use -c option to create it."
    else do
  --    Control.Monad.when (isJust mSrcAddress && isJust mDstAddress && (doCreateToken || tokenExists)) $ do
      Control.Monad.when (isJust mSrcAddress && isJust mDstAddress) $ do
        -- print tokenList
        let bNetwork = BlockchainNetwork { network = "--" ++ network, networkMagic = networkMagic, networkEra = networkEra, networkEnv = networkSocket }
        doMint bNetwork ownerName mSrcAddress sKeyFile mDstAddress policiesPath tokenList mTokenMetadata

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
  maddress <- getAddressFromFile $ getAddressFile addressesPath Payment (Owner $ capitalized $ show dstName)
  case maddress of
    Just address -> do
      let dstAddress = fromJust maddress
      putStrLn $ "Destination address : " ++ getAddress dstAddress
    _ -> putStrLn $ "No " ++ show Payment ++ " address for " ++ show dstName
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
