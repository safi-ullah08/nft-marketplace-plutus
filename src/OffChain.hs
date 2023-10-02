{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module OffChain where



import Data.Aeson (FromJSON, ToJSON)
import Data.Text
import GHC.Generics (Generic)
import Ledger hiding (singleton)
import Ledger.Scripts hiding (version)
import Ledger.Scripts.Orphans ()
import Data.Map as Map
import Plutus.Script.Utils.Ada as Ada
import Control.Monad (void)
import Control.Lens ((^?))
import Plutus.V1.Ledger.Api as Api


import Ledger.Typed.Scripts as Scripts
import Plutus.Script.Utils.Value
import Ledger.Tx.Constraints as Constraints
import Plutus.Contract as Contract
import qualified PlutusTx
import PlutusTx.Prelude hiding (unless)
import Text.Printf (printf)
import Prelude (IO)
import qualified Prelude as Pr
import Plutus.ChainIndex.Types 
import qualified Data.List.NonEmpty as NE
import Cardano.Node.Emulator.Params (pNetworkId)


import Data.List.NonEmpty (NonEmpty((:|)))

import Marketplace

type MarketplaceSchema =
  Endpoint "start" BuyParams
    .\/ Endpoint "buy" BuyParams
    .\/ Endpoint "cancel" BuyParams
   


newtype RedeemSuccess = RedeemSuccess Ledger.TxId
    deriving (Pr.Eq, Pr.Show)

data BuyParams = BuyParams
  { 
    buyFeeCollector :: PaymentPubKeyHash
  , buyFee :: Integer
  , buySeller     :: PaymentPubKeyHash
  , buyPrice      :: Integer
  , buyCurrencySymbol  :: CurrencySymbol
  , buyTokenName     :: TokenName
  }
  deriving (Generic, ToJSON, FromJSON)


PlutusTx.unstableMakeIsData ''BuyParams



start :: AsContractError e => BuyParams -> Contract w s e ()
start buyParams = do
  networkId <- pNetworkId <$> getParams
  pkh <- ownPaymentPubKeyHash
  contractPkh <- Contract.ownAddresses
  let mp =
        MarketplaceParam
          { feeCollector = unPaymentPubKeyHash $ buyFeeCollector buyParams,
            fee = buyFee buyParams
          }
      validatorInst = (typedValidator mp)
      addr = Scripts.validatorCardanoAddress networkId validatorInst
  let dat =
        MarketplaceDatum
          { seller = unPaymentPubKeyHash $ pkh,
            price = buyPrice buyParams,
            nCurrency = buyCurrencySymbol buyParams,
            nToken = buyTokenName buyParams
          }
      
      totalCost =  buyPrice buyParams  + buyFee buyParams
      nftValue = Api.singleton (buyCurrencySymbol buyParams) (buyTokenName buyParams) 1
      tx = Constraints.mustPayToTheScriptWithDatumInTx dat nftValue


  logInfo @Pr.String $ "Seller Address PKH " Pr.<> (Pr.show $ pkh)
  logInfo @Pr.String $ "Price set by seller " Pr.<> (Pr.show $ buySeller buyParams)
  logInfo @Pr.String $ "Fee Collector Address " Pr.<> (Pr.show $ buyFeeCollector buyParams)
  logInfo @Pr.String $ "Fee for the protocol " Pr.<> (Pr.show $ buyFee buyParams)

  void $ mkTxConstraints (Constraints.typedValidatorLookups (typedValidator mp)) tx
         >>= Contract.adjustUnbalancedTx >>= Contract.submitUnbalancedTx




buy :: forall w s e. AsContractError e => BuyParams -> Contract w s e ()
buy buyParams = do
  networkId <- pNetworkId <$> getParams
  pkh <- ownPaymentPubKeyHash
  let mp =
         MarketplaceParam
          { feeCollector = unPaymentPubKeyHash $ buyFeeCollector buyParams,
            fee = buyFee buyParams
          }
      validatorInst = (typedValidator mp)
      addr = Scripts.validatorCardanoAddress networkId validatorInst
  let dat =
        MarketplaceDatum
          { seller = unPaymentPubKeyHash $ buySeller buyParams,
            price = buyPrice buyParams,
            nCurrency = buyCurrencySymbol buyParams,
            nToken = buyTokenName buyParams
          }
      
      rMarketplace =  Buy

  ownUtxos <- utxosAt (addr)
  selectedUtxos <- Map.filter (findSale dat ) <$>  utxosAt (addr)
  logInfo @Pr.String $ "Selected UTXOs " Pr.<> (Pr.show $ selectedUtxos)
  logInfo @Pr.String $ "Selected UTXOs " Pr.<> (Pr.show $ ownUtxos)



  if Map.null selectedUtxos
    then logInfo @Pr.String $ "no such utxo"
    else do
      let orefs = fst <$> Map.toList selectedUtxos
        
          oref = Pr.head orefs
          constraints = [ Constraints.mustSpendOutputFromTheScript oref rMarketplace
                        , Constraints.mustBeSignedBy pkh
                        , Constraints.mustPayToPubKey (buySeller buyParams) (Ada.lovelaceValueOf (price dat))
                        , Constraints.mustPayToPubKey (buyFeeCollector buyParams) (Ada.lovelaceValueOf (fee mp))
                   ]
          tx = mconcat constraints
      
      utx <- mkTxConstraints ( Constraints.typedValidatorLookups (typedValidator mp)
                              Pr.<> Constraints.unspentOutputs selectedUtxos
                               ) tx >>= Contract.adjustUnbalancedTx
              
      RedeemSuccess . getCardanoTxId  <$> submitUnbalancedTx utx
      logInfo @Pr.String $ "collected payment"

cancel :: forall w s e. AsContractError e => BuyParams -> Contract w s e ()
cancel buyParams = do
  networkId <- pNetworkId <$> getParams
  pkh <- ownPaymentPubKeyHash
  let mp =
         MarketplaceParam
          { feeCollector = unPaymentPubKeyHash $ buyFeeCollector buyParams,
            fee = buyFee buyParams
          }
      validatorInst = (typedValidator mp)
      addr = Scripts.validatorCardanoAddress networkId validatorInst
      ownUtxos = utxosAt (addr)
  let dat =
        MarketplaceDatum
          { seller = unPaymentPubKeyHash $ pkh,
            price = buyPrice buyParams,
            nCurrency = buyCurrencySymbol buyParams,
            nToken = buyTokenName buyParams
          }
      
      rMarketplace =  Cancel

  selectedUtxos <- Map.filter (findSale dat ) <$> ownUtxos

  logInfo @Pr.String $ "Selected UTXOs " Pr.<> (Pr.show $ selectedUtxos)


  if Map.null selectedUtxos
    then logInfo @Pr.String $ "no such utxo"
    else do
      let orefs = fst <$> Map.toList selectedUtxos
        
          oref = Pr.head orefs
          constraints = [ Constraints.mustSpendOutputFromTheScript oref rMarketplace
                        , Constraints.mustBeSignedBy (buySeller buyParams)
                    ]
          tx = mconcat constraints
      
      utx <- mkTxConstraints ( Constraints.typedValidatorLookups (typedValidator mp)
                              Pr.<> Constraints.unspentOutputs selectedUtxos
                               ) tx >>= Contract.adjustUnbalancedTx
              
      RedeemSuccess . getCardanoTxId  <$> submitUnbalancedTx utx
      logInfo @Pr.String $ "collected payment"

findSale :: MarketplaceDatum  -> DecoratedTxOut -> Bool
findSale datum o = case o of
  ScriptDecoratedTxOut {_decoratedTxOutScriptDatum = (_, datumFromQuery)} -> 
    case datumFromQuery ^? datumInDatumFromQuery of
      Nothing -> False
      Just (Datum e) -> case PlutusTx.fromBuiltinData e of
        Nothing -> False
        Just d ->  nCurrency d == nCurrency datum && nToken d == nToken datum
  PublicKeyDecoratedTxOut {} -> False 
  _ -> False





endpoints :: Contract () MarketplaceSchema Text ()
endpoints = awaitPromise (start' `select` buy' `select` cancel'  ) >> endpoints
  where
    start' = endpoint @"start" start
    buy' = endpoint @"buy" buy
    cancel' = endpoint @"cancel" cancel
