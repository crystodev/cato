import           Baseutils            (capitalized)
import           Configuration.Dotenv (defaultConfig, loadFile)
import           Control.Monad        (filterM, mapM_, void, when)
import           Data.Maybe           (fromJust, isJust)
import           Data.Semigroup       ((<>))
import           Options.Applicative
import           Policy               (Policy (..), getPoliciesPath, getPolicy,
                                       getPolicyId)
import           System.Directory     (doesDirectoryExist, doesFileExist,
                                       getDirectoryContents)
import           System.Environment
import Wallet
    ( Owner (..) )

type OwnerName = String
type PolName = String
data Options = Options OwnerName PolName

parsePol :: Parser PolName
parsePol = argument str (metavar "POLICY" <> value "")

parseOwner :: Parser OwnerName
parseOwner = strOption
          ( long "owner"
         <> short 'o'
         <> metavar "OWNER"
         <> help "address owner name" )

parseOptions :: Parser Options
parseOptions = Options <$> parseOwner <*> parsePol

main :: IO ()
main = getPolicyInfo =<< execParser opts
  where
    opts = info (parseOptions <**> helper)
      ( fullDesc
     <> progDesc "Get info about minting policy for owner"
     <> header "policy-info - a simple tool to display minting policy info" )

getPolicyInfo :: Options -> IO ()
getPolicyInfo (Options ownerName policyName) = do
  loadFile defaultConfig
  addressPath <- getEnv "ADDRESSES_PATH"
  policiesFolder <- getEnv "POLICIES_FOLDER"
  let owner = Owner (capitalized ownerName)
  let policiesPath = getPoliciesPath addressPath owner policiesFolder

  if policyName /= "" then
    printPolicyInfo owner policiesPath policyName
  else do
    lPolicies <- listDirs policiesPath
    let policies = filter (`notElem` [".", ".."]) lPolicies
    mapM_ (printPolicyInfo owner policiesPath) policies

-- list sub folders
listDirs :: FilePath -> IO [FilePath]
listDirs path = getDirectoryContents path >>= filterM (doesDirectoryExist . (++) path)

-- list files
listFiles :: FilePath -> IO [FilePath]
listFiles path = getDirectoryContents path >>= filterM (doesFileExist . (++) path)

-- print policy infos
printPolicyInfo :: Owner -> FilePath -> String -> IO ()
printPolicyInfo owner policiesPath policyName  = do
  mPolicy <- getPolicy policiesPath policyName
  if isJust mPolicy then do
    let policyPath = policiesPath  ++ policyName ++ "/"
    putStrLn "========================================================================================================="
    putStrLn $ "Policy " ++ policyName ++ " owned by " ++ show owner
    putStrLn "---------------------------------------------------------------------------------------------------------"
    putStrLn $ "Policy path : " ++ policyPath
    putStrLn $ "Policy id : " ++ getPolicyId(fromJust mPolicy)
    printTokensInfo (fromJust mPolicy)
  else
    putStrLn $ "No policy " ++ capitalized policyName ++ " found for " ++ show owner

--print tokens infos for policy
printTokensInfo :: Policy -> IO ()
printTokensInfo policy = do
  rc <- doesDirectoryExist (tokensPath policy)
  when rc $ do
    lTokens  <- listFiles (tokensPath policy)
    let tokens = filter (`notElem` [".", ".."]) lTokens
    mapM_ printTokenInfo tokens

printTokenInfo :: String -> IO ()
printTokenInfo token = do
    putStrLn "---------------------------------------------------------------------------------------------------------"
    putStrLn $ "Token name : " ++ token
