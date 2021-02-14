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

newtype OwnerName = OwnerName { getOwnerName :: String} deriving (Eq, Show)
newtype PolName = PolName { getPol :: String} deriving (Eq, Show)

data Options = Options OwnerName PolName

parsePol :: Parser PolName
parsePol = PolName <$> argument str (metavar "POLICY")

parseOwner :: Parser OwnerName
parseOwner = OwnerName <$> strOption
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
doCreatePolicy (Options ownerName polName) = do
  loadFile defaultConfig
  addressPath <- getEnv "ADDRESSES_PATH"
  policiesFolder <- getEnv "POLICIES_FOLDER"
  let owner = Owner (capitalized $ getOwnerName ownerName)
  let policyName = getPol polName
  putStrLn $ "Creating policy " ++ policyName ++ " for " ++ getOwner owner ++ "\n"
  let policiesPath = getPoliciesPath addressPath owner policiesFolder
  putStrLn $ "Policy path : " ++ policiesPath ++ "/" ++ policyName

  mPolicy <- createPolicy policyName policiesPath
  if isJust mPolicy then do
    putStrLn $ "Policy id : " ++ getPolicyId(fromJust mPolicy)
  else
    putStrLn $ "Policy " ++ capitalized policyName ++ " not created"
