import Options.Applicative
import Data.Semigroup ((<>))
import TokenUtils ( getTokenId, readTokensFromFile, Token(..), writeTokensToFile )

data TokenOptions = TokenOptions
  { policyName :: String
  , policyId :: String
  , amount :: Int
  , prefix :: String
  , csvFile :: FilePath
  , jsonFile :: FilePath
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
{-
  let policyName = "CryptoFruit"
  let policyId = "6386a3dfbfb7e6a04834d578fff70a3b48e3313b94bc394c87464b9b"
  let amount = 1
  let prefix = "Cry"
  let outFile = "fruits.json"
  genTokens policyName policyId amount prefix outFile

genTokens :: String -> String -> Int -> String -> FilePath -> IO ()
genTokens policyName policyId amount prefix outFile = do
  let templateDir = "data/"
      tokenListFile = templateDir ++ "fruits.csv"
      tokenJsonFile = templateDir ++ outFile
-} 

  tokenNames <- readFile (csvFile tokenOptions)
  print (words tokenNames)
  let tokenList = [Token { tokenName = prefix tokenOptions++tokenName, tokenAmount = amount tokenOptions,
          tokenId = getTokenId (policyId tokenOptions) (prefix tokenOptions++tokenName), tokenPolicyName = policyName tokenOptions } | tokenName <- words tokenNames]
  writeTokensToFile tokenList (jsonFile tokenOptions)