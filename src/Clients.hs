{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
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
    startOracleInstance :: ContractActivationArgs (ActivateContractParams OracleParams)
    startOracleInstance = ContractActivationArgs {caID = ActivateContractParams (OracleParams "BCD") "StartO", caWallet = Just (knownWallet 3)}

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
    oracle = Oracle ac pkh
    updateOracleInstance :: ContractActivationArgs (ActivateContractParams (Oracle, Integer))
    updateOracleInstance = ContractActivationArgs {caID = ActivateContractParams (oracle, 456) "UpdateO", caWallet = Just (knownWallet 3)}

updateOracleTest' :: CurrencySymbol -> IO ()
updateOracleTest' = updateOracleTest . testParams

testParams :: CurrencySymbol -> (AssetClass, PubKeyHash)
testParams cur = (AssetClass (cur, "BCD"), "2e0ad60c3207248cecd47dbde3d752e0aad141d6b8f81ac2c6eca27c")

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
    oracle = Oracle ac pkh
    getOracleInstance :: ContractActivationArgs (ActivateContractParams Oracle)
    getOracleInstance = ContractActivationArgs {caID = ActivateContractParams oracle "GetO", caWallet = Just (knownWallet 4)}

getOracleTest' :: CurrencySymbol -> IO ()
getOracleTest' = getOracleTest . testParams
