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
import           Data.Aeson                           (Value (Bool))
import qualified Data.ByteString.Lazy                 as LBS
import qualified Data.ByteString.Short                as SBS
import           GeneralParams
import qualified Ledger.Typed.Scripts                 as Scripts
import           Plutus.Contract.Error                (AssertionError (unAssertionError))
import qualified Plutus.Script.Utils.V2.Scripts       as PSU.V2
import qualified Plutus.Script.Utils.V2.Typed.Scripts as PlutusV2
import qualified Plutus.Script.Utils.Value            as Value
import           Plutus.V1.Ledger.Value               (assetClassValueOf)
import qualified Plutus.V2.Ledger.Api                 as PlutusV2
import           Plutus.V2.Ledger.Contexts            (getContinuingOutputs)
import qualified PlutusTx
import           PlutusTx.Prelude                     as P hiding
                                                            (Semigroup (..),
                                                            unless, (.))
import           Prelude                              (putStrLn, (.))
import           System.IO                            (FilePath, IO, print)
import           Utility                              (getInput,
                                                       getValidityTokenAC,
                                                       parseOutputDatumInTxOut)

instance Eq FNFTDatum where
  FNFTDatum fractionAC emittedFractions nftAC remainedFractions == FNFTDatum fractionAC' emittedFractions' nftAC' remainedFractions' =
    fractionAC == fractionAC' &&
    emittedFractions == emittedFractions' &&
    nftAC == nftAC' && remainedFractions == remainedFractions'

-- This is the validator function of FNFT Contract
{-# INLINABLE mkValidator #-}
mkValidator :: () -> FNFTDatum -> FNFTRedeemer -> PlutusV2.ScriptContext -> Bool
mkValidator _ inputDatum redeem scriptContext =
  case redeem of
    Claim ->
      if forgedFractionTokens < 0
        then validateReturningAndBurning
               forgedFractionTokens
               inputDatum
               scriptContext
        else False
    Withdraw amount ->
      traceIfFalse
        "[Plutus Error]: Not enought FNFT to withdraw"
        (remainedFractions inputDatum >= amount) &&
      traceIfFalse
        "[Plutus Error]: wrong datum"
        (checkOutputDatum
           True
           (parseOutputDatumInTxOut info getMainOutput)
           amount) &&
      traceIfFalse
        "[Plutus Error]: wrong FNFT value"
        (checkOutputFNFTValue True getMainOutput amount) &&
      traceIfFalse
        "[Plutus Error]: wrong NFT value"
        (checkOutputNFTValue getMainOutput) &&
      traceIfFalse
        "[Plutus Error]: wrong validation token value"
        (checkOutputValidation getMainOutput)
    Deposit amount ->
      traceIfFalse
        "[Plutus Error]: wrong datum"
        (checkOutputDatum
           False
           (parseOutputDatumInTxOut info getMainOutput)
           amount) &&
      traceIfFalse
        "[Plutus Error]: wrong FNFT value"
        (checkOutputFNFTValue False getMainOutput amount) &&
      traceIfFalse
        "[Plutus Error]: wrong NFT value"
        (checkOutputNFTValue getMainOutput) &&
      traceIfFalse
        "[Plutus Error]: wrong validation token value"
        (checkOutputValidation getMainOutput)
  where
    info :: PlutusV2.TxInfo
    info = PlutusV2.scriptContextTxInfo scriptContext
    txMint = PlutusV2.txInfoMint info
    forgedFractionTokens = assetClassValueOf txMint (fractionAC inputDatum)
    getMainOutput :: PlutusV2.TxOut
    getMainOutput = head (getContinuingOutputs scriptContext)
    checkOutputDatum :: Bool -> Maybe FNFTDatum -> Integer -> Bool
    checkOutputDatum isWithdraw outputDatum amount =
      case outputDatum of
        Just (FNFTDatum fractionAC' emittedFractions' nftAC' remainedFractions') ->
          traceIfFalse
            "[Plutus Error]: datum fractionAC incorrect"
            (fractionAC' == fractionAC inputDatum) &&
          traceIfFalse
            "[Plutus Error]: emittedFractions incorrect"
            (emittedFractions' == emittedFractions inputDatum) &&
          traceIfFalse
            "[Plutus Error]: remainedFractions incorrect"
            (remainedFractions' ==
             if isWithdraw
               then remainedFractions inputDatum - amount
               else remainedFractions inputDatum + amount) &&
          traceIfFalse
            "[Plutus Error]: nftAC incorrect"
            (nftAC' == nftAC inputDatum)
        Nothing -> traceError "[Plutus Error]: output datum must not be empty"
    checkOutputFNFTValue :: Bool -> PlutusV2.TxOut -> Integer -> Bool
    checkOutputFNFTValue isWithdraw txout amount = do
      let value' = PlutusV2.txOutValue txout
          flatValues = Value.flattenValue value'
          fnftCS = fst $ Value.unAssetClass $ fractionAC inputDatum
          fnftTN = snd $ Value.unAssetClass $ fractionAC inputDatum
      case find
             (\(cs, tn, amt) ->
                cs == fnftCS &&
                tn == fnftTN &&
                amt ==
                if isWithdraw
                  then remainedFractions inputDatum - amount
                  else remainedFractions inputDatum + amount)
             flatValues of
        Nothing -> False
        Just _  -> True
    checkOutputNFTValue :: PlutusV2.TxOut -> Bool
    checkOutputNFTValue txout = do
      let value' = PlutusV2.txOutValue txout
          flatValues = Value.flattenValue value'
          nftCS = fst $ Value.unAssetClass $ nftAC inputDatum
          nftTN = snd $ Value.unAssetClass $ nftAC inputDatum
      case find
             (\(cs, tn, amt) -> cs == nftCS && tn == nftTN && amt == 1)
             flatValues of
        Nothing -> False
        Just _  -> True
    checkOutputValidation :: PlutusV2.TxOut -> Bool
    checkOutputValidation txout = do
      let value' = PlutusV2.txOutValue txout
          flatValues = Value.flattenValue value'
          validationCS = fst $ Value.unAssetClass $ fractionAC inputDatum
          validationTN = validityTokenName
      case find
             (\(cs, tn, amt) ->
                cs == validationCS && tn == validationTN && amt == 1)
             flatValues of
        Nothing -> False
        Just _  -> True

{-# INLINEABLE validateReturningAndBurning #-}
validateReturningAndBurning ::
     Integer -> FNFTDatum -> PlutusV2.ScriptContext -> Bool
validateReturningAndBurning forgedFractionTokens fntDatum scriptContext =
  traceIfFalse "[Plutus Error]: Fraction tokens not burned" fractionTokensBurnt &&
  traceIfFalse "[Plutus Error]: Validity token not burned" validityTokenBurned
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
