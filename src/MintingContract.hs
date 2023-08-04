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
  ) where

import           Cardano.Api.Shelley             (PlutusScript (..),
                                                  PlutusScriptV2, displayError,
                                                  writeFileTextEnvelope)
import Codec.Serialise ( serialise, Serialise )
import qualified Data.ByteString.Lazy            as BSL
import qualified Data.ByteString.Lazy            as LBS
import qualified Data.ByteString.Short           as BSS
import qualified Data.ByteString.Short           as SBS
import GeneralParams ( FNFTDatum(FNFTDatum), fractionTokenName )
import qualified Ledger.Typed.Scripts            as Scripts
import Plutus.Script.Utils.V2.Contexts
    ( findDatum,
      TxInInfo(txInInfoOutRef),
      TxInfo(txInfoInputs, txInfoOutputs, txInfoMint),
      ScriptContext,
      TxOutRef,
      ownCurrencySymbol )
import           Plutus.Script.Utils.V2.Scripts  (scriptCurrencySymbol)
import qualified Plutus.Script.Utils.Value       as Value
import qualified Plutus.V2.Ledger.Api            as PlutusV2
import qualified Plutus.V2.Ledger.Contexts       as PlutusV2
import qualified PlutusTx
import PlutusTx.Prelude as P
    ( (>>=),
      Bool,
      Integer,
      Maybe(..),
      Either(Right, Left),
      (++),
      check,
      find,
      traceError,
      ($),
      (&&),
      traceIfFalse,
      Eq((==)),
      Ord((<), (>)),
      BuiltinData,
      snd,
      (||),
      any,
      head,
      fromMaybe )
import           Prelude                         (FilePath, IO, last, print,
                                                  putStrLn, (.))
import Plutus.V1.Ledger.Value (assetClass)

data MintingRedeemer
  = InitialMint TxOutRef
  | Burn

PlutusTx.makeLift ''MintingRedeemer

PlutusTx.makeIsDataIndexed ''MintingRedeemer [('InitialMint, 0), ('Burn, 1)]

-- This is the validator function of Minting Contract
{-# INLINABLE mkNFTPolicy #-}
mkNFTPolicy :: () -> MintingRedeemer -> PlutusV2.ScriptContext -> Bool
mkNFTPolicy _ redeem scriptContext =
  case redeem of
    InitialMint utxo -> validateInitialMint utxo scriptContext
    Burn             -> validateBurn scriptContext

{-# INLINEABLE extractMintedAmt #-}
extractMintedAmt ::
     PlutusV2.TokenName -> [(PlutusV2.TokenName, Integer)] -> Integer
extractMintedAmt exTokenName mintedTokens =
  let token =
        find (\(exTokenName', _) -> exTokenName' == exTokenName) mintedTokens
   in case token of
        Just a -> snd a
        _      -> traceError "Can not find any token"

{-# INLINEABLE extractMintedTokens #-}
extractMintedTokens ::
     PlutusV2.CurrencySymbol
  -> PlutusV2.Value
  -> [(PlutusV2.TokenName, Integer)]
extractMintedTokens mintedSymbol txMint =
  [(tn, amt) | (cs, tn, amt) <- Value.flattenValue txMint, cs == mintedSymbol]

{-# INLINABLE hasUTxO #-}
hasUTxO :: TxOutRef -> TxInfo -> Bool
hasUTxO utxo info = any (\i -> txInInfoOutRef i == utxo) $ txInfoInputs info

{-# INLINEABLE validateInitialMint #-}
validateInitialMint ::TxOutRef -> ScriptContext -> Bool
validateInitialMint utxo ctx =
  traceIfFalse
    "Minted ammount fractions not positive"
    (fractionTokensMintedAmount > 0) &&
  traceIfFalse "UTxO used for token name isn't spent" checkUTxOSpent
  && traceIfFalse "Script datum incorrectly built" (checkOutputDatum $ parseOutputDatumInTxOut getTxOutHasAsset)
  where
    info :: PlutusV2.TxInfo
    info = PlutusV2.scriptContextTxInfo ctx
    txMint = txInfoMint info
    txOutputs = txInfoOutputs info
    ownCS = ownCurrencySymbol ctx
    extractedMintedTokens = extractMintedTokens ownCS txMint
    fractionTokensMintedAmount =
      extractMintedAmt fractionTokenName extractedMintedTokens
    checkUTxOSpent = hasUTxO utxo info
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
    checkOutputDatum :: Maybe FNFTDatum -> Bool
    checkOutputDatum outputDatum =
      case outputDatum of
        Just (FNFTDatum fractionAC emittedFractions) ->
          traceIfFalse
            "datum fractionAC incorrect" (fractionAC == assetClass ownCS fractionTokenName) &&
          traceIfFalse
            "emittedFractions incorrect" (emittedFractions == fractionTokensMintedAmount)
        Nothing -> traceError "[Plutus Error]: output datum must not be empty"
   
validateBurn :: ScriptContext -> Bool
validateBurn ctx =
  traceIfFalse
    "Burned amount fractions not negative"
    (fractionTokensMintedAmount < 0)
  where
    info :: PlutusV2.TxInfo
    info = PlutusV2.scriptContextTxInfo ctx
    txMint = txInfoMint info
    ownCS = ownCurrencySymbol ctx
    extractedMintedTokens = extractMintedTokens ownCS txMint
    fractionTokensMintedAmount =
      extractMintedAmt fractionTokenName extractedMintedTokens

policy :: () -> Scripts.MintingPolicy
policy params =
  PlutusV2.mkMintingPolicyScript $
    $$(PlutusTx.compile [|| wrap ||])
    `PlutusTx.applyCode`
  PlutusTx.liftCode params
  where
    wrap params' = Scripts.mkUntypedMintingPolicy $ mkNFTPolicy params'

script :: () -> PlutusV2.Script
script params = PlutusV2.unMintingPolicyScript $ policy params

scriptSBS :: () -> SBS.ShortByteString
scriptSBS params = SBS.toShort $ LBS.toStrict $ serialise $ script params

mintNFT :: () -> PlutusScript PlutusScriptV2
mintNFT params = PlutusScriptSerialised $ scriptSBS params

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
mkWrappedNFTPolicy = wrapPolicy $ mkNFTPolicy ()

policyCode ::
     PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> ())
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

mintingContractSymbol :: () -> PlutusV2.CurrencySymbol
mintingContractSymbol op = scriptCurrencySymbol $ policy op
