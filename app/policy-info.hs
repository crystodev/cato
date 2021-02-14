import           Baseutils            (capitalized)
import           Configuration.Dotenv (defaultConfig, loadFile)
import           Control.Monad        (filterM, mapM_, void, when, unless)
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

-- parsing options
data InfoPolicy = InfoPolicy {
  ownerName :: String
, compact :: Bool
} deriving Show

newtype PolName = PolName { getPol :: String} deriving (Eq, Show)
data Options = Options InfoPolicy PolName

parsePol :: Parser PolName
parsePol = PolName <$> argument str (metavar "POLICY" <> value "")

parseInfoPolicy :: Parser InfoPolicy
parseInfoPolicy = InfoPolicy
          <$> strOption ( long "owner" <> short 'o' <> metavar "OWNER" <> help "address owner name" )
          <*> switch ( long "compact" <> short 'c' <> help "displays in compact mode" )

parseOptions :: Parser Options
parseOptions = Options <$> parseInfoPolicy <*> parsePol

main :: IO ()
main = getPolicyInfo =<< execParser opts
  where
    opts = info (parseOptions <**> helper)
      ( fullDesc
     <> progDesc "Get info about minting policy for owner"
     <> header "policy-info - a simple tool to display minting policy info" )

getPolicyInfo :: Options -> IO ()
getPolicyInfo (Options infoPolicy polName) = do
  loadFile defaultConfig
  addressPath <- getEnv "ADDRESSES_PATH"
  policiesFolder <- getEnv "POLICIES_FOLDER"
  let owner = Owner (capitalized $ ownerName infoPolicy)
  let compactMode = compact infoPolicy
  let policyName = getPol polName
  let policiesPath = getPoliciesPath addressPath owner policiesFolder

  if policyName /= "" then
    printPolicyInfo compactMode owner policiesPath policyName
  else do
    lPolicies <- listDirs policiesPath
    let policies = filter (`notElem` [".", ".."]) lPolicies
    mapM_ (printPolicyInfo compactMode owner policiesPath) policies

-- list sub folders
listDirs :: FilePath -> IO [FilePath]
listDirs path = getDirectoryContents path >>= filterM (doesDirectoryExist . (++) path)

-- list files
listFiles :: FilePath -> IO [FilePath]
listFiles path = getDirectoryContents path >>= filterM (doesFileExist . (++) path)

-- print policy infos
printPolicyInfo :: Bool -> Owner -> FilePath -> String -> IO ()
printPolicyInfo compactMode owner policiesPath policyName  = do
  mPolicy <- getPolicy policiesPath policyName
  if isJust mPolicy then do
    let policyPath = policiesPath  ++ policyName ++ "/"
    putStrLn "========================================================================================================="
    putStrLn $ "Policy " ++ policyName ++ " owned by " ++ getOwner owner
    putStrLn "---------------------------------------------------------------------------------------------------------"
    putStrLn $ "Policy path : " ++ policyPath
    putStrLn $ "Policy id : " ++ getPolicyId(fromJust mPolicy)
    unless compactMode $ do
        printTokensInfo (fromJust mPolicy)
  else
    putStrLn $ "No policy " ++ capitalized policyName ++ " found for " ++ getOwner owner

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
