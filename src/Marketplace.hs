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
{-# LANGUAGE NamedFieldPuns #-}
 {-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Marketplace
  (module Marketplace) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Map as Map
import GHC.Generics (Generic)
import Ledger.Scripts 
import Ledger 
import Ledger.Tx.Constraints.OnChain.V2 as Constraints
import Plutus.V2.Ledger.Contexts as PV2
import Plutus.Script.Utils.Typed as Scripts
import Plutus.V2.Ledger.Api 
import Plutus.Script.Utils.Value (symbols)
import Ledger.Typed.Scripts (ScriptContextV2)
import Plutus.Script.Utils.Ada as Ada
import qualified Plutus.Script.Utils.V2.Typed.Scripts.Validators as V2UtilsTypeScripts
import qualified Plutus.Script.Utils.V2.Scripts as Scripts
import qualified Plutus.Script.Utils.V2.Typed.Scripts as Scripts hiding (validatorHash)
import PlutusTx.Builtins


import qualified Ledger.Tx.Constraints as Constraints
import Plutus.Contract
import qualified PlutusTx
import PlutusTx.Prelude  as PP
import qualified Prelude as Pr


import Codec.Serialise

data MarketplaceDatum = MarketplaceDatum
  { seller     :: PubKeyHash
  , price      :: Integer
  , nCurrency  :: CurrencySymbol
  , nToken     :: TokenName
  }
  deriving (Pr.Show)

-- This needs to be refactored to makeIsDataIndex for production
PlutusTx.unstableMakeIsData ''MarketplaceDatum
PlutusTx.makeLift ''MarketplaceDatum
  

data MarketplaceRedeemer = Buy | Cancel
  deriving (Pr.Show)

PlutusTx.makeIsDataIndexed ''MarketplaceRedeemer [('Buy, 1), ('Cancel, 2)]
PlutusTx.makeLift ''MarketplaceRedeemer

data MarketplaceParam = MarketplaceParam
  { 
    feeCollector :: PubKeyHash,  -- Change To Pub Key Hash
    fee :: Integer
    
  }
  deriving (Pr.Eq, Pr.Ord, Pr.Show, Generic, ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''MarketplaceParam
PlutusTx.makeLift ''MarketplaceParam




{-# INLINEABLE mkValidator #-}

mkValidator :: MarketplaceParam -> MarketplaceDatum -> MarketplaceRedeemer -> PV2.ScriptContext -> Bool
mkValidator mp dat redeemer ctx = 
  case redeemer of
    Buy ->    traceIfFalse "Fee is 1 ADA" checkFee &&
              traceIfFalse "Price mismatch" checkPrice && 
              traceIfFalse "No NFT attached to utxo" checkInputForNFT
       
    Cancel -> traceIfFalse "Must be Signed by Seller" signedBySeller
       
  where
    info :: PV2.TxInfo
    info = PV2.scriptContextTxInfo ctx

    signedBySeller :: Bool
    signedBySeller = PV2.txSignedBy info $  seller dat

    getListOfTokensInInput :: [CurrencySymbol]
    getListOfTokensInInput = symbols $ PV2.valueSpent info

    checkInputForNFT :: Bool
    checkInputForNFT = (nCurrency dat) `elem` getListOfTokensInInput

    checkFee :: Bool
    checkFee = fromInteger (Ada.getLovelace (Ada.fromValue (PV2.valuePaidTo info ( feeCollector mp))))  == (fromInteger $ fee mp) 

    checkPrice :: Bool
    checkPrice = fromInteger (Ada.getLovelace (Ada.fromValue (PV2.valuePaidTo info (seller dat))))  == (fromInteger $ price dat )



data MarketplaceData

instance V2UtilsTypeScripts.ValidatorTypes MarketplaceData where
  type DatumType MarketplaceData = MarketplaceDatum
  type RedeemerType MarketplaceData = MarketplaceRedeemer

typedValidator :: MarketplaceParam -> V2UtilsTypeScripts.TypedValidator MarketplaceData
typedValidator ep =go (ep) where
    go = V2UtilsTypeScripts.mkTypedValidatorParam @MarketplaceData
        $$(PlutusTx.compile [|| mkValidator ||])
        $$(PlutusTx.compile [|| wrap ||])
    wrap = Scripts.mkUntypedValidator 

validator :: MarketplaceParam -> Scripts.Validator
validator = V2UtilsTypeScripts.validatorScript . typedValidator

validatorHash :: MarketplaceParam -> ValidatorHash
validatorHash = V2UtilsTypeScripts.validatorHash . typedValidator
