module Transaction ( buildBurnTransaction, buildMintTransaction, buildSendTransaction, calculateBurnFees, calculateMintFees, calculateSendFees, createAddress, getTransactionFile,
  FileType(Draft, OkFee, Sign), getUtxoFromWallet, getTokenIdFromName,
  signBurnTransaction, signMintTransaction, signSendTransaction, submitTransaction, Utxo(Utxo, raw, utxos, nbUtxos, tokens), TransactionType(..) ) where

import GHC.IO.Exception (ExitCode( ExitSuccess ))
import System.Directory ( createDirectoryIfMissing, doesFileExist )
import System.IO ( hGetContents)
import System.Process ( createProcess, env, proc, std_out, StdStream(CreatePipe), waitForProcess )
import Data.Maybe ( isNothing, isJust, fromJust, fromMaybe )
import Data.List ( delete, foldl', intercalate, isSuffixOf, find )
import Data.Aeson (decode)
import qualified Data.ByteString.Lazy.Char8 as B
import Tokutils ( Address, AddressType(Payment, Stake), BlockchainNetwork(BlockchainNetwork, network, networkMagic, networkEra, networkEnv),
  calculateTokensBalance, getAddress, getAddressFile, getVkeyFile, getProtocolKeyDeposit, Policy(Policy, policyScript, policyVkey, policySkey, policyId), uglyParse )

data Utxo = Utxo {
    raw :: String
  , utxos :: [String]
  , nbUtxos :: Int
  , tokens :: [(String, Int)]
} deriving (Show)

data TransactionType = Burn | Mint | Send  deriving (Eq, Show)


-- utilities ----------------
data FileType = Draft | OkFee | Sign

getTransactionFileExt :: FileType -> String
getTransactionFileExt Draft = ".txbody.draft"
getTransactionFileExt OkFee = ".txbody.ok-fee"
getTransactionFileExt Sign = ".tx.sign"

getTransactionFile :: Maybe String -> FileType -> FilePath 
getTransactionFile Nothing fileType = "/tmp/lovelace" ++ getTransactionFileExt fileType
getTransactionFile (Just token) fileType = "/tmp/" ++ token ++ getTransactionFileExt fileType

-- get token in token list from toke name
-- retrieve first tokenId matching for token name
getTokenIdFromName :: String -> [(String, Int)] -> Maybe String
getTokenIdFromName tokenName tokenValues = do
  -- extract token list from list of (token, value)
  let tokenList = map fst tokenValues
  find (tokenName `isSuffixOf` ) tokenList

-- check if ada and token amount are enough for transaction 
checkSendTransactionAmount :: Int -> Maybe String -> Int -> TransactionType -> String -> Utxo -> Int -> Maybe Int -> IO Bool 
checkSendTransactionAmount adaAmount token tokenAmount transactionType policyId utxo fee keyDeposit
  | adaAmount < 0 || tokenAmount < 0 = do
    putStrLn "Cannot send negative amount"
    return False
  | adaAmount == 0 && tokenAmount == 0 = do
    putStrLn "Nothing to send"
    return False
--  else if isNothing policyId then do
--    putStrLn "No policy id"
--    return False
  | nbUtxos utxo == 0 = do
    putStrLn "No utxo found"
    return False
  | isNothing keyDeposit = do
    putStrLn "No keydeposit value found"
    return False    
  | adaAmount /=0 && adaAmount * 1000000 < fromJust keyDeposit && fee /= 0 = do
    putStrLn $ "To few lovelace for transaction " ++ show adaAmount ++ " ADA ; you need at least " ++ show (fromJust keyDeposit) ++ " lovelace for transaction"
    return False
  | otherwise =
    return True

-- check if ada and token balances are above spending
checkSendTransactionBalance :: [(String, Int)] -> String -> Int -> Int -> Int -> TransactionType -> IO(Bool, [(String, Int)])
checkSendTransactionBalance tokenValues assetId fee lovelaceAmount tokenAmount transactionType = do
  let adaId = "lovelace"
  let balances = calculateTokensBalance tokenValues
  let balances2 = map (\(t, b) -> if t == adaId then (t, b-fee-lovelaceAmount) else (t, b)) balances
  if snd (head (filter(\(t, b) -> t == adaId) balances2)) < 0 then do
    putStrLn $ "The address does not have " ++ show(fee+lovelaceAmount) ++ " lovelaces ( " ++ show(fee+ div lovelaceAmount 1000000) ++ " ada )" 
    return (False, [("",0)])
  else if transactionType /= Mint then do
    let balances3 = map (\(t, b) -> if t == assetId then (t, b-tokenAmount) else (t, b)) balances2
    if snd (head (filter(\(t, b) -> t == assetId) balances3)) < 0 then do
      putStrLn $ "The address does not have " ++ show tokenAmount ++ assetId
      return (False, [("",0)])
    else
      return(True, balances3)
  else
    return(True, balances2)

-- join key value in string
joinkv :: String -> (String,Int) -> String
joinkv acc (key, value) = acc ++ " +" ++ show value ++ " " ++ key

-- build transaction
buildTransaction :: TransactionType -> BlockchainNetwork -> Address -> Address -> Int -> Maybe String -> Int -> String -> Utxo -> [(String,Int)] -> Int -> FilePath -> IO Bool 
buildTransaction Burn bNetwork srcAddress _ _ token tokenAmount policyId utxo balances fee outFile = do
  buildBurnTransaction bNetwork srcAddress token tokenAmount policyId utxo balances fee outFile
buildTransaction Mint bNetwork srcAddress dstAddress _ token tokenAmount policyId utxo _ fee outFile = do
  buildMintTransaction bNetwork srcAddress dstAddress token tokenAmount policyId utxo fee outFile
buildTransaction Send bNetwork srcAddress dstAddress adaAmount token tokenAmount policyId utxo _ fee outFile = do
  buildSendTransaction bNetwork srcAddress dstAddress adaAmount token tokenAmount policyId utxo fee outFile


-- build transfer transaction for token
buildTxIn :: [String] -> [String]
buildTxIn utxos = concat ["--tx-in":[u] | u <- utxos]

buildTransferTransaction :: TransactionType -> BlockchainNetwork -> Address -> Address -> Int -> Maybe String -> Int -> String -> Utxo -> Int -> FilePath -> IO Bool 
buildTransferTransaction transactionType bNetwork srcAddress dstAddress adaAmount token tokenAmount policyId utxo fee outFile = do
  keyDeposit <- getProtocolKeyDeposit bNetwork
  -- need to check isJust keyDeposit
  bool <- checkSendTransactionAmount adaAmount token tokenAmount transactionType policyId utxo fee keyDeposit
  if bool then do
    let adaId = "lovelace"
    let lovelaceAmount = if adaAmount == 0 && (transactionType == Mint || transactionType == Send) then fromJust keyDeposit else adaAmount * 1000000
    let assetId = policyId ++ "." ++ fromMaybe "" token
    (rc, balances) <- checkSendTransactionBalance (tokens utxo) assetId fee lovelaceAmount tokenAmount transactionType
    if rc then do
      let txOutSrc = foldl' joinkv srcAddress (reverse balances)
      let txOutDst = dstAddress ++ "+" ++ show lovelaceAmount ++ " " ++ adaId ++ (if isNothing token then "" else "+" ++ show tokenAmount ++ " " ++ assetId)
      ttl <- calculateTTL bNetwork
      let runParams = ["transaction", "build-raw", networkEra bNetwork, "--fee", show fee] ++ buildTxIn (utxos utxo) ++
            ["--ttl", show ttl, "--tx-out", txOutDst, "--tx-out", txOutSrc] ++ 
            (if transactionType == Mint then ["--mint", show tokenAmount ++" "++assetId] else []) ++ ["--out-file", outFile] 
      (_, Just hout, _, ph) <- createProcess (proc "cardano-cli" runParams){ std_out = CreatePipe }
      r <- waitForProcess ph
      return True
    else
      return False
  else
      return False

-- check burnTransaction
checkBurnTransaction :: Maybe String -> Int -> String -> Utxo -> [(String, Int)] -> IO (Maybe String) 
checkBurnTransaction tokenName tokenAmount policyId utxo balances
  | isNothing tokenName = do
    putStrLn "No token name"
    return Nothing
  | policyId == "" = do
    putStrLn "No policy id"
    return Nothing
  | nbUtxos utxo == 0 = do
    putStrLn "No utxo found"
    return Nothing
  | otherwise = do
    let tokenId = policyId ++ "." ++ fromJust tokenName
    if isNothing $ lookup tokenId balances then do
      putStrLn $ "Cannot burn token " ++ fromJust tokenName ++ " with policy " ++ policyId ++ " : no token"
      return Nothing
    else do
      let tokenBalance = lookup tokenId balances
      if isNothing tokenBalance || fromJust tokenBalance < tokenAmount then do
        putStrLn $ "Cannot burn token " ++ fromJust tokenName ++ " : not enough token"
        return Nothing
      else
        return (Just tokenId)

-- build burn transaction for token
buildBurnTransaction :: BlockchainNetwork -> Address -> Maybe String -> Int -> String -> Utxo -> [(String,Int)] -> Int -> FilePath -> IO Bool 
buildBurnTransaction bNetwork srcAddress token tokenAmount policyId utxo balances fee outFile = do
  assetId <- checkBurnTransaction token tokenAmount policyId utxo balances
  if isJust assetId then do
    keyDeposit <- getProtocolKeyDeposit bNetwork
    let adaId = "lovelace"
    (rc, balances) <- checkSendTransactionBalance (tokens utxo) (fromJust assetId) fee 0 tokenAmount Burn
    if rc then do
      let txOutSrc = foldl' joinkv srcAddress (reverse balances)
      ttl <- calculateTTL bNetwork
      let runParams = ["transaction", "build-raw", networkEra bNetwork, "--fee", show fee] ++ buildTxIn (utxos utxo) ++
            ["--ttl", show ttl, "--tx-out", txOutSrc] ++ 
            ["--mint", show (-tokenAmount) ++" "++fromJust assetId] ++ ["--out-file", outFile]
      (_, Just hout, _, ph) <- createProcess (proc "cardano-cli" runParams){ std_out = CreatePipe }
      r <- waitForProcess ph
      return True
    else
      return False
  else
    return False

-- build mint transaction for token
buildMintTransaction :: BlockchainNetwork -> Address -> Address -> Maybe String -> Int -> String -> Utxo -> Int -> FilePath -> IO Bool 
buildMintTransaction bNetwork srcAddress dstAddress token tokenAmount policyId utxo fee draftFile = do
  if isJust token && tokenAmount /= 0 then
    buildTransferTransaction Mint bNetwork srcAddress dstAddress 0 token tokenAmount policyId utxo fee draftFile
  else
    return False

-- build send transaction for token
buildSendTransaction :: BlockchainNetwork -> Address -> Address -> Int -> Maybe String -> Int -> String -> Utxo -> Int -> FilePath -> IO Bool 
buildSendTransaction bNetwork srcAddress dstAddress adaAmount token tokenAmount policyId utxo fee draftFile = do
  if isJust token && tokenAmount /= 0 then
    buildTransferTransaction Send bNetwork srcAddress dstAddress adaAmount token tokenAmount policyId utxo fee draftFile
  else
    return False

-- calculate fee for transaction
calculateFees :: TransactionType -> BlockchainNetwork -> Address  -> Address -> Int -> Maybe String -> Int -> String -> Utxo -> [(String,Int)] -> FilePath -> IO (Maybe Int)
calculateFees transactionType bNetwork srcAddress dstAddress adaAmount token tokenAmount policyId utxo balances protparamsFile = do
  let draftFile = getTransactionFile token Draft
  bool <- buildTransaction transactionType bNetwork srcAddress dstAddress adaAmount token tokenAmount policyId utxo balances 0 draftFile
  if not bool then do
    putStrLn "Failed to build transaction"
    return Nothing
  else do
    let runParams = ["transaction", "calculate-min-fee", "--tx-body-file", draftFile, "--tx-in-count", show(nbUtxos utxo),
          "--tx-out-count", "1", "--witness-count", "1", "--byron-witness-count", "0", "--protocol-params-file", protparamsFile]
    (_, Just hout, _, ph) <- createProcess (proc "cardano-cli" runParams){ std_out = CreatePipe }
    r <- waitForProcess ph
    rc <- hGetContents hout
    let mintFee = read(head (words rc))::Int
    return (Just mintFee)   

-- calculate fee for burn transaction
calculateBurnFees :: BlockchainNetwork -> Address -> Maybe String -> Int -> String -> Utxo -> [(String,Int)] -> FilePath -> IO (Maybe Int)
calculateBurnFees bNetwork srcAddress = calculateFees Burn bNetwork srcAddress srcAddress 0

-- calculate fee for mint transaction
calculateMintFees :: BlockchainNetwork -> Address -> Maybe String -> Int -> String -> Utxo -> FilePath -> IO (Maybe Int)
calculateMintFees bNetwork srcAddress token tokenAmount policyId utxo = calculateFees Mint bNetwork srcAddress srcAddress 0 token tokenAmount policyId utxo []

-- calculate fee for send transaction
calculateSendFees :: BlockchainNetwork -> Address -> Address -> Int -> Maybe String -> Int -> String -> Utxo -> FilePath -> IO (Maybe Int)
calculateSendFees bNetwork srcAddress dstAddress adaAmount token tokenAmount policyId utxo = 
  calculateFees Send bNetwork srcAddress dstAddress adaAmount token tokenAmount policyId utxo []

-- calculate network TTL
calculateTTL :: BlockchainNetwork -> IO Int
calculateTTL bNetwork = do
  let forwardSlot=300
  let runParams = ["query", "tip", network bNetwork, show(networkMagic bNetwork)]
  (_, Just hout, _, ph) <- createProcess (proc "cardano-cli" runParams){ std_out = CreatePipe }
  r <- waitForProcess ph
  jsonData <- hGetContents hout
  let slot = forwardSlot + read(uglyParse jsonData "slotNo")::Int 
  return slot

-- create address for owner
createAddress :: BlockchainNetwork -> AddressType -> FilePath -> String -> IO (Maybe Address) 
createAddress bNetwork addressType addressesPath ownerName = do
  let vkFile = getVkeyFile addressesPath addressType ownerName
  boolvk <- doesFileExist vkFile
  if not boolvk then do
    putStrLn $ "verification key missing for " ++ ownerName
    return Nothing
  else do
    let addrFile = getAddressFile addressesPath addressType ownerName
    boolad <- doesFileExist addrFile
    if boolad then do
      putStrLn $ "address already exists for " ++ ownerName
      getAddress addrFile
    else do
      let netName = network bNetwork
      let netMagic = networkMagic bNetwork
      let saddressType = if addressType == Payment then "address" else "stake-address"
      let saddressPrefix = if addressType == Payment then "payment" else "stake"
      let runParams = [saddressType, "build", netName, show netMagic, "--" ++ saddressPrefix ++ "-verification-key-file", vkFile, "--out-file", addrFile]
      (_, Just rc, _, ph) <- createProcess (proc "cardano-cli" runParams){ std_out = CreatePipe }
      r <- waitForProcess ph
      getAddress addrFile

-- get utxo from wallet
getUtxoFromWallet :: BlockchainNetwork -> Address -> IO Utxo
getUtxoFromWallet bNetwork address = do
  let netName = network bNetwork
  let netMagic = networkMagic bNetwork
  let netEra = networkEra bNetwork
  let envParam = Just [("CARDANO_NODE_SOCKET_PATH", networkEnv bNetwork)]
  let runParams = ["query", "utxo", netName, show netMagic, netEra, "--address", address]
  (_, Just hout, _, ph) <- createProcess (proc "cardano-cli" runParams ) { env = envParam } {std_out = CreatePipe }
  r <- waitForProcess ph
  raw <- hGetContents hout

  -- split lines and remove first to lines
  let txList = drop 2  . lines $ raw
  -- get transaction id and index from start of lists and join with #
  let rawUtxos = [ take 2 (words tx) | tx <- txList]
  let utxos = fmap (intercalate "#" ) rawUtxos
-- get tokens from end of lists and build tuples (token, amount)
  let rawTokens = [ drop 2 (words tx) | tx <- txList]
  let ltokens = fmap (filter (/= "+")) rawTokens
  let tokens = concatMap parseTokens ltokens
  return Utxo {raw=raw, utxos=utxos, nbUtxos= length utxos, tokens=tokens}

-- parse transactions list
parseTokens :: [String] -> [(String, Int)]
parseTokens [] = []
parseTokens [x] = []
parseTokens (x:y:xs) = (y,read x::Int):parseTokens xs

-- sign transaction
signTransaction :: TransactionType -> BlockchainNetwork -> FilePath -> Maybe Policy -> FilePath -> FilePath -> IO ()
signTransaction Send bNetwork skeyFile _ okFeeFile signFile = do
  let runParams = ["transaction", "sign", network bNetwork, show(networkMagic bNetwork), "--signing-key-file", skeyFile,
        "--tx-body-file", okFeeFile, "--out-file", signFile]
  (_, Just hout, _, ph) <- createProcess (proc "cardano-cli" runParams){ std_out = CreatePipe }
  r <- waitForProcess ph
  return ()
signTransaction _ bNetwork skeyFile (Just policy) okFeeFile signFile = do
  let runParams = ["transaction", "sign", network bNetwork, show(networkMagic bNetwork), "--signing-key-file", skeyFile,
        "--signing-key-file", policySkey policy, "--script-file", policyScript policy, "--tx-body-file", okFeeFile, "--out-file", signFile]
  (_, Just hout, _, ph) <- createProcess (proc "cardano-cli" runParams){ std_out = CreatePipe }
  r <- waitForProcess ph
  return ()
signTransaction _ _ _ _ _ _ = do
  print "No policy provided"
  return ()

-- sign burn transaction
signBurnTransaction :: BlockchainNetwork -> FilePath -> Policy -> FilePath -> FilePath -> IO ()
signBurnTransaction bNetwork skeyFile policy = signTransaction Burn bNetwork skeyFile (Just policy)

-- sign mint transaction
signMintTransaction :: BlockchainNetwork -> FilePath -> Policy -> FilePath -> FilePath -> IO ()
signMintTransaction bNetwork skeyFile policy = signTransaction Mint bNetwork skeyFile (Just policy)

-- sign send transaction
signSendTransaction :: BlockchainNetwork -> FilePath -> FilePath -> FilePath -> IO ()
signSendTransaction bNetwork skeyFile = signTransaction Send bNetwork skeyFile Nothing

-- submit signed transaction on the network
submitTransaction :: BlockchainNetwork -> FilePath -> IO Bool
submitTransaction bNetwork signFile = do
  let envParam = Just [("CARDANO_NODE_SOCKET_PATH", networkEnv bNetwork)]
  let runParams = ["transaction", "submit", network bNetwork, show(networkMagic bNetwork), "--tx-file", signFile]
  (_, Just hout, _, ph) <- createProcess (proc "cardano-cli" runParams) { env = envParam }{ std_out = CreatePipe }
  r <- waitForProcess ph
  return (r == ExitSuccess)
