{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module DeployMarketplace where

import Cardano.Api
import Cardano.Api.Shelley (PlutusScript (..))
import Codec.Serialise (serialise)
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short qualified as SBS
import Ledger qualified
import Plutus.V2.Ledger.Api as Api
import Marketplace

writeValidator :: FilePath -> Api.Validator -> IO (Either (FileError ()) ())
writeValidator file = writeFileTextEnvelope @(PlutusScript PlutusScriptV2) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . Api.unValidatorScript

writeBountyEscrowScript :: IO (Either (FileError ()) ())
writeBountyEscrowScript =
  writeValidator "testnet/marketplace.plutus" $
    Marketplace.validator $
      MarketplaceParam
        { 
          feeCollector = Ledger.PubKeyHash "60b4e01c495b533ead0f17ac4d178e9241115d177bc0999c50b9c52c32",
          fee = 10000 -- 1 percent
        }
