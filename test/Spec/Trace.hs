{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Spec.Trace where

import Control.Lens
import Control.Monad.Freer.Extras as Extras
import Data.Default
import Data.Functor (void)
import qualified Data.Map as Map
import Data.Monoid (Last (..))
import Data.Text (Text)
import Ledger.Ada
import Ledger.Value
import Oracle
import Plutus.Contract.Test
import Plutus.Trace
import Test.Tasty

testOracleParams :: (OracleParams, Integer)
testOracleParams = (OracleParams (TokenName "ABC") 1_000_000, 123)

assertFailedTransactions :: TracePredicate
assertFailedTransactions = Plutus.Contract.Test.not assertNoFailedTransactions

startUpdateGetTrace :: EmulatorTrace ()
startUpdateGetTrace = do
  Extras.logInfo @String "START TRACE"
  h1 <- activateContractWallet (knownWallet 1) $ runOracle testOracleParams
  void $ waitNSlots 2
  oracle <- getOracle h1
  void $ waitNSlots 2
  h2 <- activateContractWallet (knownWallet 1) (updateEndpoints oracle)
  callEndpoint @"update" h2 (oracle, 456)
  void $ waitNSlots 2
  h3 <- activateContractWallet (knownWallet 2) (getEndpoint oracle)
  callEndpoint @"get" h3 ()
  void $ waitNSlots 2
  h4 <- activateContractWallet (knownWallet 1) (updateEndpoints oracle)
  callEndpoint @"update" h4 (oracle, 789)
  void $ waitNSlots 2
  Extras.logInfo @String "END OF TRACE"
  where
    getOracle :: ContractHandle (Last Oracle) StartSchema Text -> EmulatorTrace Oracle
    getOracle h = do
      l <- observableState h
      case l of
        Last Nothing -> throwError $ GenericError "ORACLE NOT CREATED"
        Last (Just oracle) -> Extras.logInfo @String (show oracle) >> return oracle

startUpdateGetConfig :: EmulatorConfig
startUpdateGetConfig =
  def & initialChainState
    .~ ( Left $
           Map.fromList
             [ (knownWallet 1, lovelaceValueOf 10_000_000),
               (knownWallet 2, lovelaceValueOf 10_000_000)
             ]
       )

startUpdateGetTest :: TestTree
startUpdateGetTest =
  checkPredicateOptions
    (defaultCheckOptions & emulatorConfig .~ startUpdateGetConfig)
    "succesfull start, update, getting"
    ( walletFundsChange (knownWallet 1) (lovelaceValueOf (-1_000_000))
        .&&. walletFundsChange (knownWallet 2) (lovelaceValueOf (-1_000_000))
        .&&. assertNoFailedTransactions
    )
    startUpdateGetTrace

-------------------------------------------------------------------------------------
badUpdateTrace :: EmulatorTrace ()
badUpdateTrace = do
  Extras.logInfo @String "START TRACE"
  h1 <- activateContractWallet (knownWallet 1) $ runOracle testOracleParams
  void $ waitNSlots 2
  oracle <- getOracle h1
  void $ waitNSlots 2
  h2 <- activateContractWallet (knownWallet 2) (updateEndpoints oracle)
  callEndpoint @"update" h2 (oracle, 456)
  void $ waitNSlots 2
  Extras.logInfo @String "END OF TRACE"
  where
    getOracle :: ContractHandle (Last Oracle) StartSchema Text -> EmulatorTrace Oracle
    getOracle h = do
      l <- observableState h
      case l of
        Last Nothing -> throwError $ GenericError "ORACLE NOT CREATED"
        Last (Just oracle) -> Extras.logInfo @String (show oracle) >> return oracle

badUpdateTest :: TestTree
badUpdateTest =
  checkPredicateOptions
    (defaultCheckOptions & emulatorConfig .~ startUpdateGetConfig)
    "the user cannot update someone else's oracle"
    ( walletFundsChange (knownWallet 1) (lovelaceValueOf (-2_000_000))
        .&&. walletFundsChange (knownWallet 2) (lovelaceValueOf 0)
        .&&. assertFailedTransactions
    )
    badUpdateTrace

--------------------------------------------------------------------------------

testOracle :: Oracle
testOracle =
  Oracle
    { oAssetClass = AssetClass ("aa", "bb"),
      oOwner = "owner",
      oFee = 10_000_000
    }

getNotExistOracle :: EmulatorTrace ()
getNotExistOracle = do
  Extras.logInfo @String "START TRACE"
  h2 <- activateContractWallet (knownWallet 2) (getEndpoint testOracle)
  callEndpoint @"get" h2 ()
  void $ waitNSlots 2
  Extras.logInfo @String "END OF TRACE"

getNotExistOracleConfig :: EmulatorConfig
getNotExistOracleConfig =
  def & initialChainState
    .~ ( Left $
           Map.fromList
             [ (knownWallet 2, lovelaceValueOf 10_000_000)
             ]
       )

getNotExistOracleTest :: TestTree
getNotExistOracleTest =
  checkPredicateOptions
    (defaultCheckOptions & emulatorConfig .~ startUpdateGetConfig)
    "the user cannot get value from unexist oracle"
    (walletFundsChange (knownWallet 2) (lovelaceValueOf 0))
    getNotExistOracle
