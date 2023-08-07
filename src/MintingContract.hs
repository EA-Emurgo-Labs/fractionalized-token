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

module MintingContract
  ( mintNFT
  , saveNFTCode
  , policy
  , mintingContractSymbol
  , MintingRedeemer(..)
  ) where

import           Cardano.Api.Shelley             (PlutusScript (..),
                                                  PlutusScriptV2, displayError,
                                                  writeFileTextEnvelope)
import           Codec.Serialise                 (Serialise, serialise)
import qualified Data.ByteString.Lazy            as BSL
import qualified Data.ByteString.Lazy            as LBS
import qualified Data.ByteString.Short           as BSS
import qualified Data.ByteString.Short           as SBS
import           GeneralParams                   (FNFTDatum (FNFTDatum, fractionAC, emittedFractions), validityTokenName)
import qualified Ledger.Typed.Scripts            as Scripts
import           Plutus.Script.Utils.V2.Contexts (ScriptContext, TxInfo (txInfoInputs, txInfoMint, txInfoOutputs),
                                                  TxOutRef, ownCurrencySymbol)
import           Plutus.Script.Utils.V2.Scripts  (scriptCurrencySymbol)
import qualified Plutus.Script.Utils.Value       as Value
import           Plutus.V1.Ledger.Value          (assetClass)
import qualified Plutus.V2.Ledger.Api            as PlutusV2
import qualified PlutusTx
import           PlutusTx.Prelude                as P (Bool, BuiltinData,
                                                       Either (Left, Right),
                                                       Eq ((==)), Maybe (..),
                                                       Ord ((<), (>)), check,
                                                       find, snd, traceError,
                                                       traceIfFalse, ($), (&&),
                                                       (++), (>>=), negate, length)
import           Prelude                         (Bool (..), FilePath, IO,
                                                  print, putStrLn, (.))
import           Utility                         (calculateFractionTokenNameHash,
                                                  extractMintedAmt,
                                                  extractMintedTokens, getInput,
                                                  hasUTxO,
                                                  parseOutputDatumInTxOut)
import Ledger (scriptHashAddress, Script (Script))

data MintingRedeemer
  = InitialMint TxOutRef
  | Burn

PlutusTx.makeLift ''MintingRedeemer

PlutusTx.makeIsDataIndexed ''MintingRedeemer [('InitialMint, 0), ('Burn, 1)]

-- This is the validator function of Minting Contract
{-# INLINABLE mkNFTPolicy #-}
mkNFTPolicy :: MintingRedeemer -> PlutusV2.ScriptContext -> PlutusV2.ValidatorHash -> Bool
mkNFTPolicy redeem scriptContext vh =
  case redeem of
    InitialMint utxo -> validateInitialMint vh utxo scriptContext
    Burn             -> validateBurn scriptContext

{-# INLINEABLE validateInitialMint #-}
validateInitialMint :: PlutusV2.ValidatorHash -> TxOutRef -> ScriptContext -> Bool
validateInitialMint fnftvh utxo ctx =
  traceIfFalse
    "[Plutus Error]: Minted ammount fractions not positive"
    (fractionTokensMintedAmount > 0)
  && traceIfFalse "[Plutus Error]: UTxO used for token name isn't spent" checkUTxOSpent
  && traceIfFalse
    "[Plutus Error]: Script datum incorrectly built"
    (checkOutputDatum $ parseOutputDatumInTxOut info getTxOutHasAsset)
  && traceIfFalse "[Plutus Error]: Didn't mint exactly fraction tokens and one validity token" (length extractedMintedTokens == 2)
  && traceIfFalse "[Plutus Error]: Didn't mint validity token" (extractMintedAmt validityTokenName extractedMintedTokens == 1)
  where
    info :: PlutusV2.TxInfo
    info = PlutusV2.scriptContextTxInfo ctx
    txMint = txInfoMint info
    txOutputs = txInfoOutputs info
    ownCS = ownCurrencySymbol ctx
    fractionTokenName = PlutusV2.TokenName $ calculateFractionTokenNameHash utxo
    extractedMintedTokens = extractMintedTokens ownCS txMint
    fractionTokensMintedAmount =
      extractMintedAmt fractionTokenName extractedMintedTokens
    checkUTxOSpent = hasUTxO utxo info
    getTxOutHasAsset :: PlutusV2.TxOut
    getTxOutHasAsset =
      case find
             (\x -> do
                let value' = PlutusV2.txOutValue x
                    flatValues = Value.flattenValue value'
                PlutusV2.txOutAddress x == scriptHashAddress fnftvh && (
                    case find (\(cs, tn, _) -> cs == ownCS && tn == fractionTokenName) flatValues of
                      Nothing -> False
                      Just _  -> True))
             txOutputs of
        Nothing -> traceError "[Plutus Error]: cannot find the asset in output"
        Just i -> i
    -- Check output datum in case of buying the asset on market place
    checkOutputDatum :: Maybe FNFTDatum -> Bool
    checkOutputDatum outputDatum =
      case outputDatum of
        Just (FNFTDatum fractionAC emittedFractions) ->
          traceIfFalse
            "[Plutus Error]: datum fractionAC incorrect"
            (fractionAC == assetClass ownCS fractionTokenName) &&
          traceIfFalse
            "[Plutus Error]: emittedFractions incorrect"
            (emittedFractions == fractionTokensMintedAmount)
        Nothing -> traceError "[Plutus Error]: output datum must not be empty"

validateBurn :: ScriptContext -> Bool
validateBurn ctx =
    traceIfFalse "[Plutus Error]: Burned amount fractions not negative" (fractionTokensMintedAmount < 0)
    && traceIfFalse "[Plutus Error]: Didn't burn one validity token" (extractMintedAmt validityTokenName extractedMintedTokens == (-1))
    && traceIfFalse "[Plutus Error]: Fraction tokens not burned" (negate fractionTokensMintedAmount == emittedFractions (checkDatum inputDatum))
    && traceIfFalse "[Plutus Error]: Didn't burn exactly fraction tokens and one validity token" (length extractedMintedTokens == 2)
  where
    info :: PlutusV2.TxInfo
    info = PlutusV2.scriptContextTxInfo ctx
    txMint = txInfoMint info
    ownCS = ownCurrencySymbol ctx
    txInputs = getInput ownCS (txInfoInputs info)
    inputDatum =
      parseOutputDatumInTxOut info $
      PlutusV2.txInInfoResolved $ checkInput txInputs
    fractionTokenName =
      snd $ Value.unAssetClass $ fractionAC $ checkDatum inputDatum
    extractedMintedTokens = extractMintedTokens ownCS txMint
    fractionTokensMintedAmount =
      extractMintedAmt fractionTokenName extractedMintedTokens
    checkInput :: Maybe PlutusV2.TxInInfo -> PlutusV2.TxInInfo
    checkInput input =
      case input of
        Nothing -> traceError "[Plutus Error]: Not found input"
        Just a  -> a
    checkDatum :: Maybe FNFTDatum -> FNFTDatum
    checkDatum datum =
      case datum of
        Nothing -> traceError "[Plutus Error]: Can not parse datum"
        Just a  -> a

policy :: Scripts.MintingPolicy
policy fnftvh = PlutusV2.mkMintingPolicyScript
  $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.mkUntypedMintingPolicy . mkNFTPolicy

script :: PlutusV2.ValidatorHash -> PlutusV2.Script
script fnftvh = PlutusV2.unMintingPolicyScript $ policy fnftvh

scriptSBS :: PlutusV2.ValidatorHash -> SBS.ShortByteString
scriptSBS fnftvh = SBS.toShort $ LBS.toStrict $ serialise $ script fnftvh

mintNFT :: PlutusV2.ValidatorHash -> PlutusScript PlutusScriptV2
mintNFT fnftvh = PlutusScriptSerialised $ scriptSBS fnftvh

-----------------------------------------------------------------------------------------------------------
---------------------------------------- MINTING PARAMETERIZED CONTRACT -----------------------------------
-- This is another version for minting contract, it uses dynamic params to build the parameterized contract
-- We will apply it later
{-# INLINABLE wrapPolicy #-}
wrapPolicy ::
     PlutusTx.UnsafeFromData a
  => (a -> ScriptContext -> Bool)
  -> (BuiltinData -> BuiltinData -> ())
wrapPolicy f a ctx =
  check $
  f (PlutusTx.unsafeFromBuiltinData a) (PlutusTx.unsafeFromBuiltinData ctx)

{-# INLINABLE mkWrappedNFTPolicy #-}
mkWrappedNFTPolicy :: BuiltinData -> BuiltinData -> ()
mkWrappedNFTPolicy = wrapPolicy mkNFTPolicy

policyCode :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> ())
policyCode = $$(PlutusTx.compile [|| mkWrappedNFTPolicy ||])

serializableToScript :: Serialise a => a -> PlutusScript PlutusScriptV2
serializableToScript =
  PlutusScriptSerialised . BSS.toShort . BSL.toStrict . serialise

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

saveNFTCode :: IO ()
saveNFTCode =
  writeCodeToFile
    "./built-contracts/minting-parameterized-contract.json"
    policyCode

mintingContractSymbol :: PlutusV2.CurrencySymbol
mintingContractSymbol = scriptCurrencySymbol policy
