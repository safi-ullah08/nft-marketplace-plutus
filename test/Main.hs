{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where
import Marketplace
import OffChain

import Plutus.Trace.Emulator as Emulator
import Plutus.Contract.Trace as X
import Data.Default (Default (..))
import Plutus.Script.Utils.Ada as Ada
import Data.Map as Map
import Prelude
import Ledger.Value.CardanoAPI as Value
import Control.Monad
import Plutus.V1.Ledger.Api as Api
-- import Cardano.Api as Api
-- import Test.Tasty
-- import Test.Tasty.HUnit qualified as HUnit

main :: IO ()
main = return ()

test1 :: IO ()
test1 = runEmulatorTraceIO' def emCfg fundSmartContract

test2 :: IO ()
test2 = runEmulatorTraceIO' def emCfg fundAndBuy

test3 :: IO ()
test3 = runEmulatorTraceIO' def emCfg fundAndCancel


sellerWallet, buyerWallet,protocolWallet :: Wallet
sellerWallet = X.knownWallet 1 -- seller
protocolWallet = X.knownWallet 2 -- feeCollector
buyerWallet = X.knownWallet 3 -- buyer


walletConfig = Map.fromList [ (X.knownWallet 1, Value.lovelaceValueOf 1000000000000 <> Value.singleton (currency) (name) 1)
                                , (X.knownWallet 2, Value.lovelaceValueOf 1000000000000)
                                , (X.knownWallet 3, Value.lovelaceValueOf 1000000000000)
                                ]


emCfg :: EmulatorConfig
emCfg = EmulatorConfig (Left walletConfig) def
 
currency :: PolicyId
currency = "d5e6bf0500378d4f0da4e8dde6becec7621cd8cbf5cbb9b87013d4cc"

name :: AssetName
name = "NFT"


buildBuyParams :: BuyParams

buildBuyParams = BuyParams {
    buyFeeCollector = mockWalletPaymentPubKeyHash  protocolWallet
  , buyFee = 1000000
  , buySeller     = mockWalletPaymentPubKeyHash sellerWallet
  , buyPrice      = 1000000
  , buyCurrencySymbol  = "d5e6bf0500378d4f0da4e8dde6becec7621cd8cbf5cbb9b87013d4cc"
  , buyTokenName     =  "NFT"
}


fundSmartContract:: EmulatorTrace ()
fundSmartContract = do
    sellerStartHdl <- activateContractWallet sellerWallet endpoints
    void $ Emulator.waitNSlots 1
    callEndpoint @"start" sellerStartHdl buildBuyParams
    



fundAndBuy:: EmulatorTrace ()
fundAndBuy = do
    sellerStartHdl <- activateContractWallet sellerWallet endpoints
    buyerStartHdl <- activateContractWallet buyerWallet endpoints
    void $ Emulator.waitNSlots 1
    callEndpoint @"start" sellerStartHdl buildBuyParams
    void $ Emulator.waitNSlots 10
    callEndpoint @"buy" buyerStartHdl buildBuyParams
    

fundAndCancel:: EmulatorTrace ()
fundAndCancel = do
    sellerStartHdl <- activateContractWallet sellerWallet endpoints
    void $ Emulator.waitNSlots 1
    callEndpoint @"start" sellerStartHdl buildBuyParams
    void $ Emulator.waitNSlots 10
    callEndpoint @"cancel" sellerStartHdl buildBuyParams
    