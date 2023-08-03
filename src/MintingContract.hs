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
  (
    mintNFT,
    saveNFTCode,
    policy,
    mintingContractSymbol
  )
where

import           Cardano.Api.Shelley             (PlutusScript (..),
                                                  PlutusScriptV2, displayError,
                                                  writeFileTextEnvelope)
import           Codec.Serialise
import qualified Data.ByteString.Lazy            as BSL
import qualified Data.ByteString.Lazy            as LBS
import qualified Data.ByteString.Short           as BSS
import qualified Data.ByteString.Short           as SBS
import           GeneralParams
import qualified Ledger.Typed.Scripts            as Scripts
import           Plutus.Script.Utils.V2.Contexts
import           Plutus.Script.Utils.V2.Scripts  (scriptCurrencySymbol)
import qualified Plutus.Script.Utils.Value       as Value
import qualified Plutus.V2.Ledger.Api            as PlutusV2
import qualified Plutus.V2.Ledger.Contexts       as PlutusV2
import qualified PlutusTx
import           PlutusTx.Prelude                as P hiding (Semigroup (..),
                                                       unless, (.))
import           Prelude                         (FilePath, IO, print, putStrLn,
                                                  (.))
import qualified GeneralParams
-- This is the validator function of Minting Contract
{-# INLINABLE mkNFTPolicy #-}
mkNFTPolicy :: () ->() -> PlutusV2.ScriptContext -> Bool
mkNFTPolicy params _ scriptContext =
    -- traceIfFalse "[Plutus Error]: you're not the operator to mint NFT" ownOperatorTokenInInput &&
    traceIfFalse "[Plutus Error]: minted amount must be one" checkMintedAmount 
    -- traceIfFalse "[Plutus Error]: output datum is not correct" (checkOutputDatum $ parseOutputDatumInTxOut $ getTxOutHasNFT)
  where
    -- Get all info about the transaction
    info :: PlutusV2.TxInfo
    info = PlutusV2.scriptContextTxInfo scriptContext

    -- Get all inputs
    allTxIn :: [PlutusV2.TxInInfo]
    allTxIn = PlutusV2.txInfoInputs info

    -- Get all outputs
    allTxOut :: [PlutusV2.TxOut]
    allTxOut = PlutusV2.txInfoOutputs info

    -- Check whether this transaction has the operator token in inputs
    -- ownOperatorTokenInInput :: Bool
    -- ownOperatorTokenInInput =
    --   case find (
    --     \x -> Value.assetClassValueOf (PlutusV2.txOutValue $ PlutusV2.txInInfoResolved x) (operatorToken params) == 1
    --   ) allTxIn of
    --     Nothing -> False
    --     Just _  -> True

    -- Get the minted NFT
    mintedNFT :: PlutusV2.Value
    mintedNFT = PlutusV2.txInfoMint info

    -- Check the minted amount, it must be equal 1
    checkMintedAmount :: Bool
    checkMintedAmount = case Value.flattenValue mintedNFT of
      [(_, _, amount)] -> amount == 1
      _                -> False

    {-
    This function will check whether the minted NFT is in the outputs or not.
    If yes, the txout will be used to parse and check whether the output datum is correct or not.
    -}
    getTxOutHasNFT :: PlutusV2.TxOut
    getTxOutHasNFT =
      case find (\x -> Value.symbols (PlutusV2.txOutValue x) == Value.symbols mintedNFT) allTxOut of
        Nothing -> traceError "[Plutus Error]: cannot find the minted NFT in output"
        Just i  -> i

    -- Parse output datum to the NFTDatumParams format
    -- parseOutputDatumInTxOut :: PlutusV2.TxOut -> Maybe NFTDatumParams
    -- parseOutputDatumInTxOut txout = case PlutusV2.txOutDatum txout of
    --   PlutusV2.NoOutputDatum       -> Nothing
    --   PlutusV2.OutputDatum od      -> PlutusTx.fromBuiltinData $ PlutusV2.getDatum od
    --   PlutusV2.OutputDatumHash odh -> case PlutusV2.findDatum odh info of
    --                                     Just od -> PlutusTx.fromBuiltinData $ PlutusV2.getDatum $ od
    --                                     Nothing -> Nothing

    -- -- Check output datum after minting
    -- checkOutputDatum :: Maybe NFTDatumParams -> Bool
    -- checkOutputDatum outputDatum = case outputDatum of
    --   Just (NFTDatumParams numTransfer currentPrice maxPrice owner salePrice) ->
    --     traceIfFalse "[Plutus Error]: number of transfers must be greater than or equal 0" (numTransfer >= 0) &&
    --     traceIfFalse "[Plutus Error]: current price must be greater than 0" (currentPrice > 0) &&
    --     traceIfFalse "[Plutus Error]: max price must be greater than 0" (maxPrice > 0) &&
    --     traceIfFalse "[Plutus Error]: owner must not be empty" (PlutusV2.getPubKeyHash owner /= "") &&
    --     traceIfFalse "[Plutus Error]: sale price must be zero in this phase" (salePrice == 0) &&
    --     traceIfFalse "[Plutus Error]: current price must be less than or equal max price" (currentPrice <= maxPrice)

    --   Nothing -> traceError "[Plutus Error]: output datum must not be empty"

policy :: () -> Scripts.MintingPolicy
policy params = PlutusV2.mkMintingPolicyScript $
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
wrapPolicy :: PlutusTx.UnsafeFromData a
           => (a -> ScriptContext -> Bool)
           -> (BuiltinData -> BuiltinData -> ())
wrapPolicy f a ctx =
  check $ f
      (PlutusTx.unsafeFromBuiltinData a)
      (PlutusTx.unsafeFromBuiltinData ctx)

{-# INLINABLE mkWrappedNFTPolicy #-}
mkWrappedNFTPolicy :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedNFTPolicy tkpl' tn' = wrapPolicy $ mkNFTPolicy ()
  where
    tokenPolicy :: PlutusV2.CurrencySymbol
    tokenPolicy = PlutusTx.unsafeFromBuiltinData tkpl'

    tn :: PlutusV2.TokenName
    tn = PlutusTx.unsafeFromBuiltinData tn'

    as :: Value.AssetClass
    as = Value.AssetClass (tokenPolicy, tn)

    -- op :: OperatorParams
    -- op =  OperatorParams {operatorToken = as}

policyCode :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
policyCode = $$(PlutusTx.compile [|| mkWrappedNFTPolicy ||])

serializableToScript :: Serialise a => a -> PlutusScript PlutusScriptV2
serializableToScript = PlutusScriptSerialised . BSS.toShort . BSL.toStrict . serialise

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
saveNFTCode = writeCodeToFile "./built-contracts/minting-parameterized-contract.json" policyCode

mintingContractSymbol :: () -> PlutusV2.CurrencySymbol
mintingContractSymbol op = scriptCurrencySymbol $ policy op
