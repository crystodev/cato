import System.Environment
import Options.Applicative
import Data.Semigroup ((<>))
import Data.Maybe ( isJust, fromJust )
import Baseutils ( capitalized )
import Control.Monad (void, when, filterM, mapM_ )
import Configuration.Dotenv (loadFile, defaultConfig )
import System.Directory ( doesDirectoryExist, getDirectoryContents, doesFileExist )
import Tokutils ( getPolicy, getPolicyPath, getPolicyId, Policy(..) )

type Owner = String
type Pol = String
data Options = Options Owner Pol

parsePol :: Parser Pol
parsePol = argument str (metavar "POLICY" <> value "")

parseOwner :: Parser Owner
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
getPolicyInfo (Options owner policyName) = do
  loadFile defaultConfig
  addressPath <- getEnv "ADDRESSES_PATH"
  policiesFolder <- getEnv "POLICIES_FOLDER"
  let ownerName = capitalized owner
  let policiesPath = getPolicyPath addressPath ownerName "" policiesFolder

  if policyName /= "" then 
    printPolicyInfo ownerName policiesPath policyName
  else do
    lpolicies <- listDirs policiesPath 
    let policies = filter (`notElem` [".", ".."]) lpolicies
    mapM_ (printPolicyInfo ownerName policiesPath) policies

-- list sub folders
listDirs :: FilePath -> IO [FilePath]
listDirs path = getDirectoryContents path >>= filterM (doesDirectoryExist . (++) path)

-- list files
listFiles :: FilePath -> IO [FilePath]
listFiles path = getDirectoryContents path >>= filterM (doesFileExist . (++) path)

-- print policy infos
printPolicyInfo :: String -> FilePath -> String -> IO ()
printPolicyInfo ownerName policiesPath policyName  = do
  let policyPath = policiesPath  ++ policyName ++ "/"
  mpolicy <- getPolicy policyName policyPath
  if isJust mpolicy then do
    putStrLn "========================================================================================================="
    putStrLn $ "Policy " ++ policyName ++ " owned by " ++ ownerName
    putStrLn "---------------------------------------------------------------------------------------------------------"
    putStrLn $ "Policy path : " ++ policyPath
    putStrLn $ "Policy id : " ++ getPolicyId(fromJust mpolicy)   
    printTokensInfo (fromJust mpolicy)
  else
    putStrLn $ "No policy " ++ capitalized policyName ++ " found for " ++ ownerName

--print tokens infos for policy
printTokensInfo :: Policy -> IO ()
printTokensInfo policy = do
  rc <- doesDirectoryExist (tokensPath policy)
  when rc $ do
    ltokens  <- listFiles (tokensPath policy)
    let tokens = filter (`notElem` [".", ".."]) ltokens
    mapM_ printTokenInfo tokens

printTokenInfo :: String -> IO ()
printTokenInfo token = do
    putStrLn "---------------------------------------------------------------------------------------------------------"
    putStrLn $ "Token name : " ++ token
