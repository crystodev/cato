{-# LANGUAGE DuplicateRecordFields #-}
module Transaction ( buildBurnTransaction, buildMintTransaction, buildSendTransaction, calculateBurnFees, calculateMintFees, calculateSendFees, createAddress, getTransactionFile,
  FileType(Draft, OkFee, Sign), getUtxoFromWallet, getTokenIdFromName,
  signBurnTransaction, signMintTransaction, signSendTransaction, submitTransaction, Utxo(Utxo, raw, utxos, nbUtxos, tokens), TransactionType(..) ) where

import GHC.IO.Exception (ExitCode( ExitSuccess ))
import System.Directory ( createDirectoryIfMissing, doesFileExist )
import System.IO ( hGetContents)
import System.Process ( createProcess, env, proc, std_out, StdStream(CreatePipe), waitForProcess )
import Data.Maybe ( isNothing, isJust, fromJust, fromMaybe )
import Data.List ( delete, foldl', intercalate, isSuffixOf, find, intersperse )
import Data.Aeson (decode)
import qualified Data.ByteString.Lazy.Char8 as B8
import GHC.Generics

import Address ( Address, AddressType(Payment, Stake), getAddress, getAddressFile, getVKeyFile )
import Network ( BlockchainNetwork(..), getNetworkMagic, getNetworkEra, Tip(Tip, slotNo) )
import Policy ( Policy(..) )
import Protocol ( getProtocolMinUTxOValue )

import TokenUtils ( calculateTokensBalance, getTokenId, saveMetadata, Token(..) )

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
getTransactionFileExt Draft = ".txBody.draft"
getTransactionFileExt OkFee = ".txBody.ok-fee"
getTransactionFileExt Sign = ".tx.sign"

getTransactionFile :: Maybe String -> FileType -> FilePath 
getTransactionFile Nothing fileType = "/tmp/lovelace" ++ getTransactionFileExt fileType
getTransactionFile (Just token) fileType = "/tmp/" ++ "token" ++ getTransactionFileExt fileType

-- get token in token list from toke name
-- retrieve first tokenId matching for token name
getTokenIdFromName :: String -> [(String, Int)] -> Maybe String
getTokenIdFromName tokenName tokenValues = do
  -- extract token list from list of (token, value)
  let tokenList = map fst tokenValues
  find (tokenName `isSuffixOf` ) tokenList

checkTokensAmount :: [Token] -> Bool
checkTokensAmount = all (\token -> tokenAmount token >= 0)

someTokenAmount :: [Token] -> Bool
someTokenAmount = any (\token -> tokenAmount token > 0)

-- check if ada and token amount are enough for transaction 
checkSendTransactionAmount :: Int -> [Token] -> TransactionType -> Utxo -> Int -> Maybe Int -> IO Bool 
checkSendTransactionAmount adaAmount tokens transactionType utxo fee minUTxOValue
  | adaAmount < 0 || not (checkTokensAmount tokens) = do
    putStrLn "Cannot send negative amount"
    return False
  | adaAmount == 0 && not (someTokenAmount tokens) = do
    putStrLn "Nothing to send"
    return False
  | nbUtxos utxo == 0 = do
    putStrLn "No utxo found"
    return False
  | isNothing minUTxOValue = do
    putStrLn "No minUTxOValue value found"
    return False    
  | adaAmount /=0 && adaAmount * 1000000 < fromJust minUTxOValue && fee /= 0 = do
    putStrLn $ "Too few lovelace for transaction " ++ show adaAmount ++ " ADA ; you need at least " ++ show (fromJust minUTxOValue) ++ " lovelace for transaction"
    return False
  | otherwise =
    return True

-- compute total amount for token id
tokenAmountFromTokenList :: String -> [Token] -> Int
tokenAmountFromTokenList tokId tokenList =
  sum [tokenAmount token | token <- tokenList, tokenId token == tokId]

-- check if total amount for token 
hasNegativeAmount :: [(String, Int)] -> Bool
hasNegativeAmount tokenList = not (null ([token | (token, amount) <- tokenList, amount < 0]))

-- check if ada and token balances are above spending
checkSendTransactionBalance :: [(String, Int)] -> Int -> Int -> [Token] -> TransactionType -> IO(Bool, [(String, Int)])
checkSendTransactionBalance tokenValues fee lovelaceAmount tokenList transactionType = do
  let adaId = "lovelace"
  let assetId = if tokenList /= [] then Just (tokenId $ head tokenList) else Nothing
  let amount = if tokenList /= [] then tokenAmount $ head tokenList else 0
  let balances = calculateTokensBalance tokenValues
  let balances2 = map (\(t, b) -> if t == adaId then (t, b-fee-lovelaceAmount) else (t, b)) balances
  if snd (head (filter(\(t, b) -> t == adaId) balances2)) < 0 then do
    putStrLn $ "The address does not have " ++ show(fee+lovelaceAmount) ++ " lovelaces ( " ++ show(fee+ div lovelaceAmount 1000000) ++ " ada )" 
    return (False, [("",0)])
  else if transactionType /= Mint && someTokenAmount tokenList then do
    let balances3 = map (\(t, b) -> (t, b - tokenAmountFromTokenList t tokenList)) balances2
    if hasNegativeAmount balances3 then do
      putStrLn $ "The address does not have " ++ show amount ++ fromJust assetId
      return (False, [("",0)])
    else
      return(True, balances3)
  else
    return(True, balances2)

-- join key value in string
joinKV :: String -> (String,Int) -> String
joinKV acc (key, value) = acc ++ " +" ++ show value ++ " " ++ key

-- build transaction
buildTransaction :: TransactionType -> BlockchainNetwork -> Address -> Address -> Int -> [Token] -> Maybe String -> Utxo -> [(String,Int)] -> Int -> FilePath -> IO Bool 
buildTransaction Burn bNetwork srcAddress _ _ tokens _ utxo balances fee outFile = do
  buildBurnTransaction bNetwork srcAddress tokens utxo balances fee outFile
buildTransaction Mint bNetwork srcAddress dstAddress _ tokens mTokenMetadata utxo _ fee outFile = do
  buildMintTransaction bNetwork srcAddress dstAddress tokens mTokenMetadata utxo fee outFile
buildTransaction Send bNetwork srcAddress dstAddress adaAmount tokens _ utxo _ fee outFile = do
  buildSendTransaction bNetwork srcAddress dstAddress adaAmount tokens utxo fee outFile


-- build transfer transaction for token
buildTxIn :: [String] -> [String]
buildTxIn utxos = concat ["--tx-in":[u] | u <- utxos]

buildTxOut :: Address -> Address -> String -> String -> [String]
buildTxOut srcAddress dstAddress txOutSrc txOutDst
  | srcAddress == dstAddress = ["--tx-out", srcAddress ++ txOutSrc ++ txOutDst]
  | otherwise = ["--tx-out", srcAddress ++ txOutSrc, "--tx-out", dstAddress ++ txOutDst]

buildTransferTransaction :: TransactionType -> BlockchainNetwork -> Address -> Address -> Int -> [Token] -> Maybe String -> Utxo -> Int -> FilePath -> IO Bool 
buildTransferTransaction transactionType bNetwork srcAddress dstAddress adaAmount tokenList mTokenMetadata utxo fee outFile = do
  minUTxOValue <- getProtocolMinUTxOValue bNetwork
  bool <- checkSendTransactionAmount adaAmount tokenList transactionType utxo fee minUTxOValue
  if bool then do
    let adaId = "lovelace"
    let lovelaceAmount = if adaAmount == 0 && (transactionType == Mint || transactionType == Send) then fromJust minUTxOValue else adaAmount * 1000000
    let mintAssets = intercalate " + " [ show (tokenAmount token) ++" "++ tokenId token | token <- tokenList ]
    -- print mintAssets
    (rc, balances) <- checkSendTransactionBalance (tokens utxo) fee lovelaceAmount tokenList transactionType
    if rc then do
      let txOutSrc = foldl' joinKV "" (reverse balances)
      let txOutDst = "+" ++ show lovelaceAmount ++ " " ++ adaId ++ (if not (someTokenAmount tokenList) then "" else "+" ++ mintAssets)
      let txOut = buildTxOut srcAddress dstAddress txOutSrc txOutDst
      ttl <- calculateTTL bNetwork
      if isJust ttl then do
        metaFile <- saveMetadata $ fromMaybe "" mTokenMetadata
        let runParams = ["transaction", "build-raw"] ++ getNetworkEra bNetwork ++ ["--fee", show fee] ++ buildTxIn (utxos utxo) ++
              ["--ttl", show (fromJust ttl)] ++ txOut ++ 
              (if isJust mTokenMetadata then ["--json-metadata-no-schema", "--metadata-json-file", metaFile] else []) ++
              (if transactionType == Mint && someTokenAmount tokenList then ["--mint", mintAssets] else []) ++ ["--out-file", outFile] 
        -- print runParams
        (_, Just hOut, _, ph) <- createProcess (proc "cardano-cli" runParams){ std_out = CreatePipe }
        r <- waitForProcess ph
        return (r == ExitSuccess )
      else
        return False
    else
      return False
  else
      return False

-- check burnTransaction
checkBurnTransaction :: [Token] -> Utxo -> [(String, Int)] -> IO (Maybe String) 
checkBurnTransaction tokenList utxo balances
  | null tokenList = do
    putStrLn "No token name"
    return Nothing
  | nbUtxos utxo == 0 = do
    putStrLn "No utxo found"
    return Nothing
  | otherwise = do
    let token = head tokenList
    let tName = tokenName token
    let tId = tokenId token
    let tAmount = tokenAmount token
    if isNothing $ lookup tId balances then do
      putStrLn $ "Cannot burn token " ++ tName ++ " with id " ++ tId ++ " : no token"
      return Nothing
    else do
      let tokenBalance = lookup tId balances
      if isNothing tokenBalance || fromJust tokenBalance < tAmount then do
        putStrLn $ "Cannot burn token " ++ tName ++ " : not enough token"
        return Nothing
      else
        return (Just tId)

-- build burn transaction for token
buildBurnTransaction :: BlockchainNetwork -> Address -> [Token] -> Utxo -> [(String,Int)] -> Int -> FilePath -> IO Bool 
buildBurnTransaction bNetwork srcAddress tokenList utxo balances fee outFile = do
  assetId <- checkBurnTransaction tokenList utxo balances
  if isJust assetId then do
    minUTxOValue <- getProtocolMinUTxOValue bNetwork
    let adaId = "lovelace"
    (rc, balances) <- checkSendTransactionBalance (tokens utxo) fee 0 tokenList Burn
    if rc then do
      let txOutSrc = foldl' joinKV srcAddress (reverse balances)
      ttl <- calculateTTL bNetwork
      if isJust ttl then do
        let runParams = ["transaction", "build-raw"] ++ getNetworkEra bNetwork ++ ["--fee", show fee] ++ buildTxIn (utxos utxo) ++
              ["--ttl", show (fromJust ttl), "--tx-out", txOutSrc] ++ 
              ["--mint", show (-(tokenAmount $ head tokenList)) ++" "++fromJust assetId] ++ ["--out-file", outFile]
        (_, Just hOut, _, ph) <- createProcess (proc "cardano-cli" runParams){ std_out = CreatePipe }
        r <- waitForProcess ph
        return (r == ExitSuccess )
      else
        return False
    else
      return False
  else
    return False

-- build mint transaction for token
buildMintTransaction :: BlockchainNetwork -> Address -> Address -> [Token] -> Maybe String -> Utxo -> Int -> FilePath -> IO Bool 
buildMintTransaction bNetwork srcAddress dstAddress tokens mTokenMetadata utxo fee draftFile = do
  if someTokenAmount tokens then
    buildTransferTransaction Mint bNetwork srcAddress dstAddress 0 tokens mTokenMetadata utxo fee draftFile
  else
    return False

-- build send transaction for token
buildSendTransaction :: BlockchainNetwork -> Address -> Address -> Int -> [Token] -> Utxo -> Int -> FilePath -> IO Bool 
buildSendTransaction bNetwork srcAddress dstAddress adaAmount tokens utxo fee draftFile = do
--  if isJust token && tokenAmount /= 0 then
    buildTransferTransaction Send bNetwork srcAddress dstAddress adaAmount tokens Nothing utxo fee draftFile
--  else
--    return False

-- calculate fee for transaction
calculateFees :: TransactionType -> BlockchainNetwork -> Address  -> Address -> Int -> [Policy] -> [Token] -> Maybe String -> Utxo -> [(String,Int)] -> FilePath -> IO (Maybe Int)
calculateFees transactionType bNetwork srcAddress dstAddress adaAmount policies tokens tokenMetadata utxo balances protParamsFile = do
  let draftFile = getTransactionFile Nothing Draft
  bool <- buildTransaction transactionType bNetwork srcAddress dstAddress adaAmount tokens tokenMetadata utxo balances 0 draftFile
  if not bool then do
    putStrLn "Failed to build transaction"
    return Nothing
  else do
    let txOutCount = if srcAddress == dstAddress then 1 else 2
    let witnessCount = length policies + 1
    let runParams = ["transaction", "calculate-min-fee", "--tx-body-file", draftFile, "--tx-in-count", show(nbUtxos utxo),
          "--tx-out-count", show txOutCount] ++ ["--witness-count", show witnessCount, "--byron-witness-count", "0", "--protocol-params-file", protParamsFile]
    -- print runParams
    (_, Just hOut, _, ph) <- createProcess (proc "cardano-cli" runParams){ std_out = CreatePipe }
    r <- waitForProcess ph
    if r == ExitSuccess then do
      rc <- hGetContents hOut
      let mintFee = read(head (words rc))::Int
      return (Just mintFee)   
    else
      return Nothing 

-- calculate fee for burn transaction
calculateBurnFees :: BlockchainNetwork -> Address -> [Policy] -> [Token] -> Utxo -> [(String,Int)] -> FilePath -> IO (Maybe Int)
calculateBurnFees bNetwork srcAddress policies tokens = calculateFees Burn bNetwork srcAddress srcAddress 0 policies tokens Nothing

-- calculate fee for mint transaction
calculateMintFees :: BlockchainNetwork -> Address -> [Policy] -> [Token] -> Maybe String -> Utxo -> FilePath -> IO (Maybe Int)
calculateMintFees bNetwork srcAddress policies tokens tokenMetadata utxo = calculateFees Mint bNetwork srcAddress srcAddress 0 policies tokens tokenMetadata utxo []

-- calculate fee for send transaction
calculateSendFees :: BlockchainNetwork -> Address -> Address -> Int -> [Token] -> Utxo -> FilePath -> IO (Maybe Int)
calculateSendFees bNetwork srcAddress dstAddress adaAmount tokens utxo = 
  calculateFees Send bNetwork srcAddress dstAddress adaAmount [] tokens Nothing utxo []

-- calculate network TTL
calculateTTL :: BlockchainNetwork -> IO (Maybe Int)
calculateTTL bNetwork = do
  let forwardSlot=300
  let runParams = ["query", "tip", network bNetwork, show(networkMagic bNetwork)]
  (_, Just hOut, _, ph) <- createProcess (proc "cardano-cli" runParams){ std_out = CreatePipe }
  r <- waitForProcess ph
  jsonData <- hGetContents hOut
  let tip = decode (B8.pack jsonData) :: Maybe Tip
  if isJust tip then return $ Just $ slotNo (fromJust tip) + forwardSlot  else return Nothing

-- create address for owner
createAddress :: BlockchainNetwork -> AddressType -> FilePath -> String -> IO (Maybe Address) 
createAddress bNetwork addressType addressesPath ownerName = do
  let vkFile = getVKeyFile addressesPath addressType ownerName
  boolVK <- doesFileExist vkFile
  if not boolVK then do
    putStrLn $ "verification key missing for " ++ ownerName
    return Nothing
  else do
    let addrFile = getAddressFile addressesPath addressType ownerName
    boolAddress <- doesFileExist addrFile
    if boolAddress then do
      putStrLn $ "address already exists for " ++ ownerName
      getAddress addrFile
    else do
      let netName = network bNetwork
      let netMagic = networkMagic bNetwork
      let sAddressType = if addressType == Payment then "address" else "stake-address"
      let sAddressPrefix = if addressType == Payment then "payment" else "stake"
      let runParams = [sAddressType, "build", netName, show netMagic, "--" ++ sAddressPrefix ++ "-verification-key-file", vkFile, "--out-file", addrFile]
      (_, Just rc, _, ph) <- createProcess (proc "cardano-cli" runParams){ std_out = CreatePipe }
      r <- waitForProcess ph
      getAddress addrFile

-- get utxo from wallet
getUtxoFromWallet :: BlockchainNetwork -> Address -> IO Utxo
getUtxoFromWallet bNetwork address = do
  let netName = network bNetwork
  let netMagic = getNetworkMagic bNetwork
  let netEra = getNetworkEra bNetwork
  let envParam = Just [("CARDANO_NODE_SOCKET_PATH", networkEnv bNetwork)]
  let runParams = ["query", "utxo", netName] ++ netMagic ++ netEra ++ ["--address", address]
  (_, Just hOut, _, ph) <- createProcess (proc "cardano-cli" runParams ) { env = envParam } {std_out = CreatePipe }
  r <- waitForProcess ph
  raw <- hGetContents hOut

  -- split lines and remove first to lines
  let txList = drop 2  . lines $ raw
  -- get transaction id and index from start of lists and join with #
  let rawUtxos = [ take 2 (words tx) | tx <- txList]
  let utxos = fmap (intercalate "#" ) rawUtxos
-- get tokens from end of lists and build tuples (token, amount)
  let rawTokens = [ drop 2 (words tx) | tx <- txList]
  let lTokens = fmap (filter (/= "+")) rawTokens
  let tokens = concatMap parseTokens lTokens
  return Utxo {raw=raw, utxos=utxos, nbUtxos= length utxos, tokens=tokens}

-- parse transactions list
parseTokens :: [String] -> [(String, Int)]
parseTokens [] = []
parseTokens [x] = []
parseTokens (x:y:xs) = (y,read x::Int):parseTokens xs

-- build sign policy list
signPolicies :: [Policy] -> [String]
signPolicies [] = []
signPolicies policies = concat [["--signing-key-file", policySKey policy, "--script-file", policyScript policy] | policy <- policies]

-- sign transaction
signTransaction :: TransactionType -> BlockchainNetwork -> FilePath -> [Policy] -> FilePath -> FilePath -> IO ()
signTransaction Send bNetwork sKeyFile _ okFeeFile signFile = do
  let runParams = ["transaction", "sign", network bNetwork, show(networkMagic bNetwork), "--signing-key-file", sKeyFile,
        "--tx-body-file", okFeeFile, "--out-file", signFile]
  (_, Just hOut, _, ph) <- createProcess (proc "cardano-cli" runParams){ std_out = CreatePipe }
  r <- waitForProcess ph
  return ()
signTransaction _ _ _ [] _ _ = do
  print "No policy provided"
  return ()
signTransaction _ bNetwork sKeyFile policies okFeeFile signFile = do
  let runParams = ["transaction", "sign", network bNetwork, show(networkMagic bNetwork), "--signing-key-file", sKeyFile] ++
          signPolicies policies ++ ["--tx-body-file", okFeeFile, "--out-file", signFile]
  -- print runParams
  (_, Just hOut, _, ph) <- createProcess (proc "cardano-cli" runParams){ std_out = CreatePipe }
  r <- waitForProcess ph
  return ()

-- sign burn transaction
signBurnTransaction :: BlockchainNetwork -> FilePath -> [Policy] -> FilePath -> FilePath -> IO ()
signBurnTransaction = signTransaction Burn

-- sign mint transaction
signMintTransaction :: BlockchainNetwork -> FilePath -> [Policy] -> FilePath -> FilePath -> IO ()
signMintTransaction = signTransaction Mint

-- sign send transaction
signSendTransaction :: BlockchainNetwork -> FilePath -> FilePath -> FilePath -> IO ()
signSendTransaction bNetwork sKeyFile = signTransaction Send bNetwork sKeyFile []

-- submit signed transaction on the network
submitTransaction :: BlockchainNetwork -> FilePath -> IO Bool
submitTransaction bNetwork signFile = do
  let envParam = Just [("CARDANO_NODE_SOCKET_PATH", networkEnv bNetwork)]
  let runParams = ["transaction", "submit", network bNetwork, show(networkMagic bNetwork), "--tx-file", signFile]
  (_, Just hOut, _, ph) <- createProcess (proc "cardano-cli" runParams) { env = envParam }{ std_out = CreatePipe }
  r <- waitForProcess ph
  return (r == ExitSuccess)
