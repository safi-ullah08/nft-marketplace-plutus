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
   


newtype RedeemSuccess = RedeemSuccess TxId
    deriving (Pr.Eq, Pr.Show)

data BuyParams = BuyParams
  { 
    buyFeeCollector :: PubKeyHash
  , buyFee :: Integer
  , buySeller     :: PubKeyHash
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
  ownUtxos <- utxosAt (NE.head contractPkh)
  let mp =
        MarketplaceParam
          { feeCollector = buyFeeCollector buyParams,
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
      tx = Constraints.mustPayToTheScriptWithDatumInTx dat $ Ada.lovelaceValueOf totalCost


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
  contractPkh <- Contract.ownAddresses
  ownUtxos <- utxosAt (NE.head contractPkh)
  let mp =
        MarketplaceParam
          { feeCollector = buyFeeCollector buyParams,
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
      tx = Constraints.mustPayToTheScriptWithDatumInTx dat $ Ada.lovelaceValueOf totalCost


  logInfo @Pr.String $ "Seller Address PKH " Pr.<> (Pr.show $ pkh)
  logInfo @Pr.String $ "Price set by seller " Pr.<> (Pr.show $ buySeller buyParams)
  logInfo @Pr.String $ "Fee Collector Address " Pr.<> (Pr.show $ buyFeeCollector buyParams)
  logInfo @Pr.String $ "Fee for the protocol " Pr.<> (Pr.show $ buyFee buyParams)

  void $ mkTxConstraints (Constraints.typedValidatorLookups (typedValidator mp)) tx
         >>= Contract.adjustUnbalancedTx >>= Contract.submitUnbalancedTx

cancel :: forall w s e. AsContractError e => BuyParams -> Contract w s e ()
cancel buyParams = do
  networkId <- pNetworkId <$> getParams
  pkh <- ownPaymentPubKeyHash
  contractPkh <- Contract.ownAddresses
  ownUtxos <- utxosAt (NE.head contractPkh)
  let mp =
        MarketplaceParam
          { feeCollector = buyFeeCollector buyParams,
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
      tx = Constraints.mustPayToTheScriptWithDatumInTx dat $ Ada.lovelaceValueOf totalCost


  logInfo @Pr.String $ "Seller Address PKH " Pr.<> (Pr.show $ pkh)
  logInfo @Pr.String $ "Price set by seller " Pr.<> (Pr.show $ buySeller buyParams)
  logInfo @Pr.String $ "Fee Collector Address " Pr.<> (Pr.show $ buyFeeCollector buyParams)
  logInfo @Pr.String $ "Fee for the protocol " Pr.<> (Pr.show $ buyFee buyParams)

  void $ mkTxConstraints (Constraints.typedValidatorLookups (typedValidator mp)) tx
         >>= Contract.adjustUnbalancedTx >>= Contract.submitUnbalancedTx


endpoints :: Contract () MarketplaceSchema Text ()
endpoints = awaitPromise (start' `select` buy' `select` cancel'  ) >> endpoints
  where
    start' = endpoint @"start" start
    buy' = endpoint @"buy" buy
    cancel' = endpoint @"cancel" cancel
