import System.Environment
import Options.Applicative
import Data.Semigroup ((<>))
import Data.Maybe ( isJust, fromJust )
import Baseutils ( capitalized )
import Tokutils ( createPolicy, getPolicyPath, getPolicyId )
import Control.Monad (void)
import Configuration.Dotenv (loadFile, defaultConfig)


type Owner = String
type Pol = String
data Options = Options Owner Pol

parsePol :: Parser Pol
parsePol = argument str (metavar "POLICY")

parseOwner :: Parser Owner
parseOwner = strOption
          ( long "owner"
         <> short 'o'
         <> metavar "OWNER"
         <> help "address owner name" )

parseOptions :: Parser Options
parseOptions = Options <$> parseOwner <*> parsePol

main :: IO ()
main = doCreatePolicy =<< execParser opts
  where
    opts = info (parseOptions <**> helper)
      ( fullDesc
     <> progDesc "Create Cardano minting policy"
     <> header "create-policy - a simple minting policy creator" )

doCreatePolicy :: Options -> IO ()
doCreatePolicy (Options owner policy) = do
  loadFile defaultConfig
  addressPath <- getEnv "ADDRESSES_PATH"
  policiesFolder <- getEnv "POLICIES_FOLDER"
  let cOwner = capitalized owner
  putStrLn $ "Creating policy " ++ policy ++ " for " ++ cOwner ++ "\n"
  let policyPath = getPolicyPath addressPath cOwner policy policiesFolder
  putStrLn $ "Policy path : " ++ policyPath

  mpolicy <- createPolicy cOwner policyPath
  if isJust mpolicy then do
    putStrLn $ "Policy id : " ++ getPolicyId(fromJust mpolicy)
  else
    putStrLn $ "Policy " ++ capitalized policy ++ " not created"
