{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module FNFTContract
  ( buildFNFTContract
  , saveFNFTCode
  , validator
  , validatorHash
  ) where

import           Cardano.Api
import           Cardano.Api.Shelley                  (PlutusScript (..))
import           Codec.Serialise
import qualified Data.ByteString.Lazy                 as LBS
import qualified Data.ByteString.Short                as SBS
import           GeneralParams
import qualified Ledger.Typed.Scripts                 as Scripts
import qualified Plutus.Script.Utils.V2.Scripts       as PSU.V2
import qualified Plutus.Script.Utils.V2.Typed.Scripts as PlutusV2
import           Plutus.V1.Ledger.Value               (assetClassValueOf)
import qualified Plutus.V2.Ledger.Api                 as PlutusV2
import qualified PlutusTx
import           PlutusTx.Prelude                     as P hiding
                                                            (Semigroup (..),
                                                            unless, (.))
import           Prelude                              (putStrLn, (.))
import           System.IO                            (FilePath, IO, print)
import Utility (getValidityTokenAC)

instance Eq FNFTDatum
  where
  FNFTDatum fractionAC emittedFractions == FNFTDatum fractionAC' emittedFractions' =
    fractionAC == fractionAC' && emittedFractions == emittedFractions'

-- This is the validator function of FNFT Contract
{-# INLINABLE mkValidator #-}
mkValidator :: () -> FNFTDatum -> FNFTRedeemer -> PlutusV2.ScriptContext -> Bool
mkValidator _ inputDatum redeem scriptContext =
  case redeem of
    Claim -> 
        if forgedFractionTokens < 0 then
          validateReturningAndBurning forgedFractionTokens inputDatum scriptContext
        else False
    _ -> False
  where
    info :: PlutusV2.TxInfo
    info = PlutusV2.scriptContextTxInfo scriptContext
    txMint = PlutusV2.txInfoMint info
    forgedFractionTokens = assetClassValueOf txMint (fractionAC inputDatum)

{-# INLINEABLE validateReturningAndBurning #-}
validateReturningAndBurning ::
     Integer -> FNFTDatum -> PlutusV2.ScriptContext -> Bool
validateReturningAndBurning forgedFractionTokens fntDatum scriptContext =
  traceIfFalse "Fraction tokens not burned" fractionTokensBurnt
  && traceIfFalse "Validity token not burned" validityTokenBurned
  where
    info :: PlutusV2.TxInfo
    info = PlutusV2.scriptContextTxInfo scriptContext
    txMint = PlutusV2.txInfoMint info
    fractionTokensBurnt =
      forgedFractionTokens == negate (emittedFractions fntDatum)
    validityTokenAC = getValidityTokenAC (fractionAC fntDatum)
    validityTokenBurned = assetClassValueOf txMint validityTokenAC == -1

data ContractType

instance Scripts.ValidatorTypes ContractType where
  type DatumType ContractType = FNFTDatum
  type RedeemerType ContractType = FNFTRedeemer

typedFNFTValidator :: () -> PlutusV2.TypedValidator ContractType
typedFNFTValidator = PlutusV2.mkTypedValidatorParam @ContractType
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.mkUntypedValidator

validator :: () -> PSU.V2.Validator
validator = Scripts.validatorScript . typedFNFTValidator

validatorHash :: () -> PSU.V2.ValidatorHash
validatorHash = Scripts.validatorHash . typedFNFTValidator

script :: () -> PlutusV2.Script
script = PlutusV2.unValidatorScript . validator

scriptSBS :: () -> SBS.ShortByteString
scriptSBS = SBS.toShort . LBS.toStrict . serialise . script

buildFNFTContract :: () -> PlutusScript PlutusScriptV2
buildFNFTContract = PlutusScriptSerialised . scriptSBS

---------------------------------------------------------------------------------------------------------------
------------------------------------------------- HELPER FUNCTIONS --------------------------------------------
-- This is another version for FNFT contract, it uses dynamic params to build the parameterized contract
-- We will apply it later
{-# INLINABLE wrapValidator #-}
wrapValidator ::
     (PlutusTx.UnsafeFromData a)
  => (a -> FNFTRedeemer -> PlutusV2.ScriptContext -> Bool) -- ^
  -> (BuiltinData -> BuiltinData -> BuiltinData -> ())
wrapValidator f a b ctx =
  check $
  f (PlutusTx.unsafeFromBuiltinData a)
    (PlutusTx.unsafeFromBuiltinData b)
    (PlutusTx.unsafeFromBuiltinData ctx)

{-# INLINABLE mkWrappedValidator #-}
mkWrappedValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedValidator = wrapValidator $ mkValidator ()

validatorCode ::
     PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
validatorCode = $$(PlutusTx.compile [|| mkWrappedValidator ||])

serializableToScript :: Serialise a => a -> PlutusScript PlutusScriptV2
serializableToScript =
  PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise

-- Serialize compiled code
codeToScript :: PlutusTx.CompiledCode a -> PlutusScript PlutusScriptV2
codeToScript = serializableToScript . PlutusV2.fromCompiledCode

-- Create file with Plutus script
writeScriptToFile :: FilePath -> PlutusScript PlutusScriptV2 -> IO ()
writeScriptToFile filePath plutusScript =
  writeFileTextEnvelope filePath Nothing plutusScript >>= \case
    Left err -> print $ displayError err
    Right () -> putStrLn $ "Serialized plutus script to: " ++ filePath

-- Create file with compiled code
writeCodeToFile :: FilePath -> PlutusTx.CompiledCode a -> IO ()
writeCodeToFile filePath = writeScriptToFile filePath . codeToScript

saveFNFTCode :: IO ()
saveFNFTCode =
  writeCodeToFile
    "./built-contracts/FNFT-parameterized-contract.json"
    validatorCode
