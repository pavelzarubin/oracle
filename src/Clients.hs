{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Clients where

import Control.Monad.IO.Class
import Data.Aeson
import GHC.Generics
import Ledger
import Ledger.Value
import Network.HTTP.Req
import Oracle
import Plutus.Contract.Test
import Plutus.PAB.Webserver.Types

data ActivateContractParams t = ActivateContractParams
  { contents :: t,
    tag :: String
  }
  deriving (FromJSON, ToJSON, Generic)

startOracleTest :: IO ()
startOracleTest = runReq defaultHttpConfig $ do
  response <-
    req
      POST
      (http "localhost" /: "api" /: "contract" /: "activate")
      (ReqBodyJson startOracleInstance)
      bsResponse
      (port 9080)
  liftIO $
    putStrLn $
      if responseStatusCode response == 200
        then "oracle started " ++ (show response)
        else "oracle not started"
  where
    startOracleInstance :: ContractActivationArgs (ActivateContractParams (OracleParams, Integer))
    startOracleInstance = ContractActivationArgs {caID = ActivateContractParams (OracleParams "BCD" 500_000, 789) "StartO", caWallet = Just (knownWallet 3)}

updateOracleTest :: (AssetClass, PubKeyHash) -> IO ()
updateOracleTest (ac, pkh) = runReq defaultHttpConfig $ do
  response <-
    req
      POST
      (http "localhost" /: "api" /: "contract" /: "activate")
      (ReqBodyJson updateOracleInstance)
      bsResponse
      (port 9080)
  liftIO $
    putStrLn $
      if responseStatusCode response == 200
        then "oracle started " ++ (show response)
        else "oracle not started"
  where
    oracle :: Oracle
    oracle = Oracle ac pkh 500_000
    updateOracleInstance :: ContractActivationArgs (ActivateContractParams (Oracle, Integer))
    updateOracleInstance = ContractActivationArgs {caID = ActivateContractParams (oracle, 456) "UpdateO", caWallet = Just (knownWallet 3)}

updateOracleTest' :: IO ()
updateOracleTest' = updateOracleTest testParams

testParams :: (AssetClass, PubKeyHash)
testParams = (AssetClass ("33f24d17d7dd7a8909c0e7988a20d45284432f76202be0ba530fabe6", "BCD"), "2e0ad60c3207248cecd47dbde3d752e0aad141d6b8f81ac2c6eca27c")

getOracleTest :: (AssetClass, PubKeyHash) -> IO ()
getOracleTest (ac, pkh) = runReq defaultHttpConfig $ do
  response <-
    req
      POST
      (http "localhost" /: "api" /: "contract" /: "activate")
      (ReqBodyJson getOracleInstance)
      bsResponse
      (port 9080)
  liftIO $
    putStrLn $
      if responseStatusCode response == 200
        then "oracle started " ++ (show response)
        else "oracle not started"
  where
    oracle :: Oracle
    oracle = Oracle ac pkh 500_000
    getOracleInstance :: ContractActivationArgs (ActivateContractParams Oracle)
    getOracleInstance = ContractActivationArgs {caID = ActivateContractParams oracle "GetO", caWallet = Just (knownWallet 4)}

getOracleTest' :: IO ()
getOracleTest' = getOracleTest testParams
