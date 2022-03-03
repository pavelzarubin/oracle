{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Monad (void)
import Control.Monad.Freer (interpret)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson (FromJSON (..), Result (..), ToJSON (..), fromJSON)
import Data.Default (def)
import Data.Monoid (Last (..))
import qualified Data.OpenApi as OpenApi
import GHC.Generics (Generic)
import Ledger.Value (TokenName)
import Oracle
import Plutus.PAB.Effects.Contract.Builtin (Builtin, BuiltinHandler (contractHandler), SomeBuiltin (..))
import qualified Plutus.PAB.Effects.Contract.Builtin as Builtin
import Plutus.PAB.Events.Contract (ContractInstanceId)
import Plutus.PAB.Simulator (SimulatorEffectHandlers)
import qualified Plutus.PAB.Simulator as Simulator
import qualified Plutus.PAB.Webserver.Server as PAB.Server
import Prettyprinter (Pretty (..), viaShow)
import Wallet.Emulator.Wallet (knownWallet)

data OracleContracts = StartO OracleParams | UpdateO Oracle Integer | GetO Oracle
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, OpenApi.ToSchema)

instance Pretty OracleContracts where
  pretty = viaShow

instance Builtin.HasDefinitions OracleContracts where
  getDefinitions = []
  getSchema = \case
    StartO _ -> Builtin.endpointsToSchemas @StartSchema
    GetO _ -> Builtin.endpointsToSchemas @GetSchema
    UpdateO _ _ -> Builtin.endpointsToSchemas @UpdateSchema
  getContract = \case
    StartO op -> SomeBuiltin $ runOracle op
    GetO oracle -> SomeBuiltin $ getOracleValue oracle
    UpdateO oracle newdat -> SomeBuiltin $ updateOracle oracle newdat

handlers :: SimulatorEffectHandlers (Builtin OracleContracts)
handlers =
  Simulator.mkSimulatorHandlers def def $
    interpret (contractHandler Builtin.handleBuiltin)

waitForLast :: FromJSON a => ContractInstanceId -> Simulator.Simulation t a
waitForLast cid =
  flip Simulator.waitForState cid $ \json -> case fromJSON json of
    Success (Last (Just x)) -> Just x
    _ -> Nothing

main :: IO ()
main = void $
  Simulator.runSimulationWith handlers $ do
    Simulator.logString @(Builtin OracleContracts) "Starting oracle PAB webserver on port 9080. Press enter to exit."
    shutdown <- PAB.Server.startServerDebug

    let wallet1 = knownWallet 1
        wallet2 = knownWallet 2
        startParams =
          OracleParams "ABC"
    cidInit <- Simulator.activateContract wallet1 $ StartO startParams
    oracle <- waitForLast cidInit
    Simulator.waitNSlots 2

    void $ Simulator.activateContract wallet1 $ UpdateO oracle 123

    void $ Simulator.waitNSlots 2

    void $ Simulator.activateContract wallet2 $ GetO oracle

    Simulator.waitNSlots 2

    void $ liftIO getLine
    void $ Simulator.waitNSlots 2

    Simulator.logString @(Builtin OracleContracts) "Balances at the end of the simulation"
    b <- Simulator.currentBalances
    Simulator.logBalances @(Builtin OracleContracts) b
    shutdown
