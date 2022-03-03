{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Oracle where

import Control.Monad hiding (fmap)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Map as Map
import Data.Monoid (Last (..))
import qualified Data.OpenApi as OpenApi
import Data.Text (Text, pack)
import GHC.Generics (Generic)
import Ledger hiding (singleton)
import Ledger.Ada as Ada
import Ledger.Constraints as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import Playground.Contract (ToSchema)
import Plutus.Contract as Contract
import Plutus.Contracts.Currency
import Plutus.V1.Ledger.Value (AssetClass (..), assetClassValue, assetClassValueOf)
import qualified PlutusTx
import PlutusTx.Prelude hiding (Semigroup (..), unless)
import Prelude (Eq, Semigroup (..), Show (..), String)

minLovelace :: Value
minLovelace = lovelaceValueOf 2_000_000

data Oracle = Oracle
  { oAssetClass :: AssetClass,
    oOwner :: PubKeyHash
  }
  deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq, OpenApi.ToSchema, ToSchema)

PlutusTx.makeLift ''Oracle

data OracleRedeemer = Use | Update deriving (Show)

PlutusTx.unstableMakeIsData ''OracleRedeemer

mkOracleValidator :: Oracle -> Integer -> OracleRedeemer -> ScriptContext -> Bool
mkOracleValidator oracle dat red ctx =
  traceIfFalse "no token in input" inputHasToken
    && traceIfFalse "no token in output" outputHasToken
    && case red of
      Use -> traceIfFalse "invalid output datum" ((Just dat) == outputDatum)
      Update ->
        traceIfFalse "invalid output datum" correctOutputDatum
          && traceIfFalse "tx must be signed by owner" (txSignedBy info $ oOwner oracle)
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    ownInput :: TxOut
    ownInput = case findOwnInput ctx of
      Nothing -> traceError "no inputs"
      Just i -> txInInfoResolved i

    ownOutput :: TxOut
    ownOutput = case getContinuingOutputs ctx of
      [o] -> o
      _ -> traceError "expected only one output"

    inputHasToken :: Bool
    inputHasToken = assetClassValueOf (txOutValue ownInput) (oAssetClass oracle) == 1

    outputHasToken :: Bool
    outputHasToken = assetClassValueOf (txOutValue ownOutput) (oAssetClass oracle) == 1

    outputDatum :: Maybe Integer
    outputDatum = case txOutDatumHash ownOutput of
      Nothing -> traceError "wrong output datum"
      Just h -> case findDatum h info of
        Nothing -> traceError "datum not found"
        Just (Datum d) -> PlutusTx.fromBuiltinData d

    correctOutputDatum :: Bool
    correctOutputDatum = isJust outputDatum

data Oracling

instance Scripts.ValidatorTypes Oracling where
  type DatumType Oracling = Integer
  type RedeemerType Oracling = OracleRedeemer

typedValidator :: Oracle -> Scripts.TypedValidator Oracling
typedValidator oracle =
  Scripts.mkTypedValidator @Oracling
    ( $$(PlutusTx.compile [||mkOracleValidator||])
        `PlutusTx.applyCode` PlutusTx.liftCode oracle
    )
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @Integer @OracleRedeemer

oracleValidator :: Oracle -> Validator
oracleValidator = Scripts.validatorScript . typedValidator

oracleAddress :: Oracle -> Address
oracleAddress = scriptAddress . oracleValidator

-----------------------------------------------------------

newtype OracleParams = OracleParams
  { opTokenName :: TokenName
  }
  deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq, OpenApi.ToSchema, ToSchema)

startOracle :: OracleParams -> Contract w s Text Oracle
startOracle OracleParams {..} = do
  pkh <- Contract.ownPaymentPubKeyHash
  osc <- mapError (pack . show) (mintContract pkh [(opTokenName, 1)] :: Contract w s CurrencyError OneShotCurrency)
  let oracle =
        Oracle
          { oOwner = unPaymentPubKeyHash pkh,
            oAssetClass = AssetClass (currencySymbol osc, opTokenName)
          }
  logInfo @String $ "ORACLE STARTED " ++ show oracle
  return oracle

findOracle :: Oracle -> Contract w s Text (Maybe (TxOutRef, ChainIndexTxOut))
findOracle oracle = do
  utxos <- utxosAt (oracleAddress oracle)
  case Map.toList utxos of
    [(oref, ch)] -> return (Just (oref, ch))
    _ -> return Nothing

updateOracle :: Oracle -> Integer -> Contract () UpdateSchema Text ()
updateOracle oracle newdat = do
  m <- findOracle oracle
  let c = Constraints.mustPayToTheScript newdat $ (assetClassValue (oAssetClass oracle) 1) <> minLovelace
  case m of
    Nothing -> do
      logInfo @String "oracle not found"
      ledgerTx <- submitTxConstraints (typedValidator oracle) c
      awaitTxConfirmed $ getCardanoTxId ledgerTx
      logInfo @String $ "ORACLE UPDATED " ++ show newdat
    (Just (oref, ch)) -> do
      let lookups =
            Constraints.unspentOutputs (Map.singleton oref ch)
              <> Constraints.typedValidatorLookups (typedValidator oracle)
              <> Constraints.otherScript (oracleValidator oracle)
          tx =
            c
              <> Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData Update)
      ledgerTx <- submitTxConstraintsWith @Oracling lookups tx
      awaitTxConfirmed $ getCardanoTxId ledgerTx
      logInfo @String $ "Oracle updated " ++ show newdat

type StartSchema = Endpoint "start" OracleParams

type UpdateSchema = Endpoint "update" Integer

type GetSchema = Endpoint "get" ()

startEndpoints :: Contract () StartSchema Text Oracle
startEndpoints = awaitPromise start' >> startEndpoints
  where
    start' = endpoint @"start" startOracle

updateEndpoints :: Oracle -> Contract () UpdateSchema Text ()
updateEndpoints oracle = awaitPromise update' >> updateEndpoints oracle
  where
    update' = endpoint @"update" (updateOracle oracle)

getEndpoint :: Oracle -> Contract () GetSchema Text ()
getEndpoint oracle = awaitPromise get' >> getEndpoint oracle
  where
    get' = endpoint @"get" $ const (getOracleValue oracle)

getOracleValue :: Oracle -> Contract () GetSchema Text ()
getOracleValue oracle = do
  m <- findOracle oracle
  case m of
    Just (_, ch) -> do
      case _ciTxOutDatum ch of
        Left _ -> logError @String "BAD VALUE"
        Right (Datum d) -> case PlutusTx.fromBuiltinData d of
          Just (v :: Integer) -> logInfo @String $ "ORACLE FOUNDED. VALUE =  " ++ show v
          Nothing -> logError @String "bad datum"
    _ -> logError @String "oracle not found"

runOracle :: OracleParams -> Contract (Last Oracle) StartSchema Text ()
runOracle op = do
  oracle <- startOracle op
  tell $ Last $ Just oracle
  return ()
