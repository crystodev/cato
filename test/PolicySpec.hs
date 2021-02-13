module PolicySpec where

import Test.Hspec
import Policy 
import System.Directory ( removePathForcibly )
import Data.Maybe ( isJust, fromJust )

policySpec :: Spec
policySpec = after_ (removePathForcibly "/tmp/TestPol") $ do
  describe "policy" $ do
    let path = "/tmp/"
    let policyNameTest = "TestPol"
    let policyTest = Policy {
      policyScript =  path ++ policyNameTest ++ "/policy.script"
    , policyVKey = path ++ policyNameTest ++ "/policy.vkey"
    , policySKey = path ++ policyNameTest ++ "/policy.skey"
    , tokensPath = path ++ policyNameTest ++ "/tokens/"
    , policyId = "54b8451e6ea89f1b900dfd68328de12224989fb9dca85e844aac29aa"
    , policyName = policyNameTest
    }

    it "create the policy" $ do
      mPolicy <- createPolicy policyNameTest path
      mPolicy `shouldNotBe` Nothing
      policyName (fromJust mPolicy) `shouldBe` policyNameTest

    it "returns the policies path from the addresses path, owner name et policies folder" $ do
      let addressesPath = "/tmp/" :: FilePath
          ownerName = "AliceTest" :: String
          policiesFolder = "policies/" :: String
          policiesPath = addressesPath ++ ownerName ++ "/" ++ policiesFolder :: FilePath
      getPoliciesPath addressesPath ownerName policiesFolder `shouldBe` policiesPath

    it "returns the policy id from the policy record" $ do
      getPolicyId policyTest `shouldBe` policyId policyTest
