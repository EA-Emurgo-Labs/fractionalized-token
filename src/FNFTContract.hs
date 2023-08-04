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

module FNFTContract
  ( buildFNFTContract
  -- , saveFNFTCode
  , validator
  , validatorHash
  -- , RedeemerParams(..)
  ) where

import           Cardano.Api.Shelley                  (PlutusScript (..),
                                                       PlutusScriptV2)
import           Codec.Serialise
import qualified Data.ByteString.Lazy                 as LBS
import qualified Data.ByteString.Short                as SBS
import           GeneralParams
import qualified Ledger.Typed.Scripts                 as Scripts
import qualified Plutus.Script.Utils.V2.Scripts       as PSU.V2
import qualified Plutus.Script.Utils.V2.Typed.Scripts as PlutusV2
import qualified Plutus.Script.Utils.Value            as Value
import           Plutus.V1.Ledger.Value               (assetClassValueOf)
import qualified Plutus.V2.Ledger.Api                 as PlutusV2
import Plutus.V2.Ledger.Contexts ( ownCurrencySymbol )
import qualified Plutus.V2.Ledger.Contexts            as PlutusV2
import qualified PlutusTx
import           PlutusTx.Prelude                     as P hiding
                                                            (Semigroup (..),
                                                            unless, (.))
import           Prelude                              ((.))
import Data.List (last)

instance Eq FNFTDatum where
  {-# INLINEABLE (==) #-}
  FNFTDatum fractionAC emittedFractions ==
    FNFTDatum fractionAC' emittedFractions' =
      fractionAC == fractionAC' &&
      emittedFractions == emittedFractions'

-- This is the validator function of FNFT Contract
{-# INLINABLE mkValidator #-}
mkValidator ::
     () -> FNFTDatum -> () -> PlutusV2.ScriptContext -> Bool
mkValidator _ inputDatum _ scriptContext
  | forgedFractionTokens > 0 = validateMintingFractions  forgedFractionTokens inputDatum scriptContext
  | forgedFractionTokens < 0 = validateReturningAndBurning forgedFractionTokens inputDatum scriptContext
  | otherwise                = False
    where
      info :: PlutusV2.TxInfo
      info = PlutusV2.scriptContextTxInfo scriptContext
      txMint = PlutusV2.txInfoMint info
      forgedFractionTokens = assetClassValueOf txMint (fractionAC inputDatum)


{-# INLINEABLE validateMintingFractions #-}
validateMintingFractions :: Integer -> FNFTDatum -> PlutusV2.ScriptContext -> Bool
validateMintingFractions forgedFractionTokens fntDatum scriptContext =
  traceIfFalse "Datum not updated forging tokens" (checkOutputDatum forgedFractionTokens fntDatum (parseOutputDatumInTxOut getTxOutHasAsset))
  where
    info :: PlutusV2.TxInfo
    info = PlutusV2.scriptContextTxInfo scriptContext
    txOutputs = PlutusV2.txInfoOutputs info
    ownCS = ownCurrencySymbol scriptContext
    getTxOutHasAsset :: PlutusV2.TxOut
    getTxOutHasAsset =
      case find
             (\x ->
                head (Value.symbols (PlutusV2.txOutValue x)) == ownCS ||
                last (Value.symbols (PlutusV2.txOutValue x)) == ownCS)
             txOutputs of
        Nothing -> traceError "[Plutus Error]: cannot find the asset in output"
        Just i -> i

    -- Parse output datum to the FNFTDatum format
    parseOutputDatumInTxOut :: PlutusV2.TxOut -> Maybe FNFTDatum
    parseOutputDatumInTxOut txout =
      case PlutusV2.txOutDatum txout of
        PlutusV2.NoOutputDatum -> Nothing
        PlutusV2.OutputDatum od ->
          PlutusTx.fromBuiltinData $ PlutusV2.getDatum od
        PlutusV2.OutputDatumHash odh ->
          case PlutusV2.findDatum odh info of
            Just od -> PlutusTx.fromBuiltinData $ PlutusV2.getDatum od
            Nothing -> Nothing

    -- Check output datum in case of buying the asset on market place
    checkOutputDatum :: Integer -> FNFTDatum -> Maybe FNFTDatum -> Bool
    checkOutputDatum forgedFractionTokens' inputDatum outputDatum =
      case outputDatum of
        Just (FNFTDatum fractionAC' emittedFractions') ->
            traceIfFalse
              "datum fractionAC incorrect" (fractionAC' == fractionAC inputDatum) &&
            traceIfFalse
              "emittedFractions incorrect" (emittedFractions' == emittedFractions inputDatum + forgedFractionTokens')
        Nothing -> traceError "[Plutus Error]: output datum must not be empty"
  -- let (oldFracadaValue, _inputDatumHash) = findSingleScriptOutput valHash txInputs
  --     (newFracadaValue, outputDatumHash) = findSingleScriptOutput valHash txOutputs
  --     forgedFractionTokens = assetClassValueOf txMint fractionAC
  --     noAdaValuePreserved = noAdaValue newFracadaValue == noAdaValue oldFracadaValue
  --     FracadaDatum {fractionAC = fractionAC', emittedFractions = emittedFractions', authorizedPubKeys = authorizedPubKeys', minSigRequired = minSigRequired'} = findDatum' outputDatumHash info

  --     datumUpdated = fractionAC' == fractionAC &&
  --       authorizedPubKeys' == authorizedPubKeys &&
  --       minSigRequired' == minSigRequired &&
  --       emittedFractions' == (emittedFractions + forgedFractionTokens)

  --     scriptCount = checkScriptIOCounts valHash 1 1 sctx
  -- in debugIfFalse "Not enough signatures for minting" (validateSignatures authorizedPubKeys minSigRequired info)
  --       && debugIfFalse "Contract value not preserved" noAdaValuePreserved
  --       && debugIfFalse "Datum not updated forging tokens" datumUpdated
  --       && debugIfFalse "Script counts incorrect" scriptCount

{-# INLINEABLE validateReturningAndBurning #-}
validateReturningAndBurning :: Integer -> FNFTDatum -> PlutusV2.ScriptContext -> Bool
validateReturningAndBurning forgedFractionTokens fntDatum scriptContext =
  traceIfFalse "Fraction tokens not burned" fractionTokensBurnt
  where
      fractionTokensBurnt = forgedFractionTokens == negate (emittedFractions fntDatum)
-- FracadaDatum {fractionAC, emittedFractions} sctx@StandardContext {txMint} valHash =
--   let forgedFractionTokens = assetClassValueOf txMint fractionAC
--       validityTokenAC = getValidityTokenAC fractionAC

--       fractionTokensBurnt = (forgedFractionTokens == negate emittedFractions)
--       validityTokenBurned = checkSingleTokenIsBurned validityTokenAC txMint
--       scriptCount = checkScriptIOCounts valHash 1 0 sctx
--   in debugIfFalse "Fraction tokens not burned" fractionTokensBurnt
--         && debugIfFalse "Validity token not burned" validityTokenBurned
--         && debugIfFalse "Script counts incorrect" scriptCount



data ContractType

instance Scripts.ValidatorTypes ContractType where
  type DatumType ContractType = FNFTDatum
  type RedeemerType ContractType = ()

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
-- {-# INLINABLE wrapValidator #-}
-- wrapValidator ::
--      (PlutusTx.UnsafeFromData a)
--   => (a -> RedeemerParams -> PlutusV2.ScriptContext -> Bool) -- ^
--   -> (BuiltinData -> BuiltinData -> BuiltinData -> ())
-- wrapValidator f a b ctx =
--   check $
--   f (PlutusTx.unsafeFromBuiltinData a)
--     (PlutusTx.unsafeFromBuiltinData b)
--     (PlutusTx.unsafeFromBuiltinData ctx)

-- {-# INLINABLE mkWrappedValidator #-}
-- mkWrappedValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
-- mkWrappedValidator = wrapValidator $ mkValidator ()

-- validatorCode ::
--      PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
-- validatorCode = $$(PlutusTx.compile [|| mkWrappedValidator ||])

-- serializableToScript :: Serialise a => a -> PlutusScript PlutusScriptV2
-- serializableToScript =
--   PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise

-- -- Serialize compiled code
-- codeToScript :: PlutusTx.CompiledCode a -> PlutusScript PlutusScriptV2
-- codeToScript = serializableToScript . PlutusV2.fromCompiledCode

-- -- Create file with Plutus script
-- writeScriptToFile :: FilePath -> PlutusScript PlutusScriptV2 -> IO ()
-- writeScriptToFile filePath plutusScript =
--   writeFileTextEnvelope filePath Nothing plutusScript >>= \case
--     Left err -> print $ displayError err
--     Right () -> putStrLn $ "Serialized plutus script to: " ++ filePath

-- -- Create file with compiled code
-- writeCodeToFile :: FilePath -> PlutusTx.CompiledCode a -> IO ()
-- writeCodeToFile filePath = writeScriptToFile filePath . codeToScript

-- saveFNFTCode :: IO ()
-- saveFNFTCode =
--   writeCodeToFile
--     "./built-contracts/FNFT-parameterized-contract.json"
--     validatorCode
