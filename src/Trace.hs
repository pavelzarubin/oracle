{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Trace where

import Control.Monad.Freer.Extras as Extras
import Data.Functor (void)
import Data.Monoid (Last (..))
import Data.Text (Text)
import Ledger.Value
import Oracle
import Plutus.Contract.Test
import Plutus.Trace
import Plutus.Trace.Emulator
import Wallet.Emulator.Wallet

simpleTrace :: EmulatorTrace ()
simpleTrace = do
  let op = OracleParams (TokenName "ABC")

  h1 <- activateContractWallet (knownWallet 1) $ runOracle op
  void $ waitNSlots 2
  oracle <- getOracle h1
  void $ waitNSlots 2
  h2 <- activateContractWallet (knownWallet 1) (updateEndpoints oracle)
  callEndpoint @"update" h2 $
    OracleDatum 456
  void $ waitNSlots 2
  h3 <- activateContractWallet (knownWallet 2) (getEndpoint oracle)
  callEndpoint @"get" h3 ()
  void $ waitNSlots 2
  h4 <- activateContractWallet (knownWallet 2) (updateEndpoints oracle)
  callEndpoint @"update" h4 $ OracleDatum 789
  void $ waitNSlots 2
  callEndpoint @"get" h3 ()
  void $ waitNSlots 2
  Extras.logInfo @String "END OF TRACE"
  where
    getOracle :: ContractHandle (Last Oracle) StartSchema Text -> EmulatorTrace Oracle
    getOracle h = do
      l <- observableState h
      case l of
        Last Nothing -> getOracle h
        Last (Just oracle) -> Extras.logInfo @String (show oracle) >> return oracle

runTrace :: IO ()
runTrace = runEmulatorTraceIO simpleTrace
