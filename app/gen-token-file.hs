import           Data.Semigroup      ((<>))
import           Options.Applicative
import           TokenUtils          (Token (..), getTokenId,
                                      readTokensFromFile, writeTokensToFile)

data TokenOptions = TokenOptions
  { policyName :: String
  , policyId   :: String
  , amount     :: Int
  , prefix     :: String
  , csvFile    :: FilePath
  , jsonFile   :: FilePath
  } deriving (Eq, Show)

newtype Options = Options TokenOptions

parseToken :: Parser TokenOptions
parseToken = TokenOptions
  <$> strOption
  ( long "policy"
  <> short 'p'
  <> metavar "POLICY"
  <> help "policy name" )
  <*> strOption
  ( long "id"
  <> short 'i'
  <> metavar "ID"
  <> help "policy id" )
  <*> option auto
  ( long "amount"
  <> short 'a'
  <> metavar "AMOUNT"
  <> help "token amount"
  <> value 1)
  <*> strOption
  ( long "prefix"
  <> short 'f'
  <> metavar "PREFIX"
  <> help "token prefix" )
  <*> strOption
  ( long "csv"
  <> short 'c'
  <> metavar "CSV FILE"
  <> help "csv input file" )
  <*> strOption
  ( long "json"
  <> short 'j'
  <> metavar "JSON FILE"
  <> help "json output file" )

parseOptions :: Parser Options
parseOptions = Options <$> parseToken

main :: IO ()
main = genTokens =<< execParser opts
  where
    opts = info (parseOptions <**> helper)
      ( fullDesc
     <> progDesc "Generate Tokens file."
     <> header "gen-token-files - a token generator0 tool" )

-- generate tokens file
genTokens :: Options -> IO ()
genTokens (Options tokenOptions) = do

  tokenNames <- readFile (csvFile tokenOptions)
  print (words tokenNames)
  let tokenList = [Token { tokenName = prefix tokenOptions++tokenName, tokenAmount = amount tokenOptions,
          tokenId = getTokenId (policyId tokenOptions) (prefix tokenOptions++tokenName), tokenPolicyName = policyName tokenOptions } | tokenName <- words tokenNames]
  writeTokensToFile tokenList (jsonFile tokenOptions)
