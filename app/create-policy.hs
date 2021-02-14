import Baseutils
    ( capitalized )
import Configuration.Dotenv
    ( defaultConfig, loadFile )
import Control.Monad
    ( void )
import Data.Maybe
    ( fromJust, isJust )
import Data.Semigroup
    ( (<>) )
import Options.Applicative
import Policy
    ( createPolicy, getPoliciesPath, getPolicyId )
import System.Environment

import Wallet
    ( Owner (..) )

type OwnerName = String
type PolName = String
data Options = Options OwnerName PolName

parsePol :: Parser PolName
parsePol = argument str (metavar "POLICY")

parseOwner :: Parser OwnerName
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
  let cOwner = Owner (capitalized owner)
  putStrLn $ "Creating policy " ++ policy ++ " for " ++ show cOwner ++ "\n"
  let policiesPath = getPoliciesPath addressPath cOwner policiesFolder
  putStrLn $ "Policy path : " ++ policiesPath ++ "/" ++ policy

  mPolicy <- createPolicy policy policiesPath
  if isJust mPolicy then do
    putStrLn $ "Policy id : " ++ getPolicyId(fromJust mPolicy)
  else
    putStrLn $ "Policy " ++ capitalized policy ++ " not created"
