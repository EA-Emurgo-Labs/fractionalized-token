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

module MarketplaceContract
  ( buildMarketplaceContract
  , saveMarketplaceCode
  , validator
  , validatorHash
  , RedeemerParams(..)
  ) where

import           Cardano.Api.Shelley                  (PlutusScript (..),
                                                       PlutusScriptV2,
                                                       displayError,
                                                       writeFileTextEnvelope)
import           Codec.Serialise
import qualified Data.ByteString.Lazy                 as LBS
import qualified Data.ByteString.Short                as SBS
import           GeneralParams
import qualified Ledger.Typed.Scripts                 as Scripts
import           Plutus.Script.Utils.V2.Contexts      (valuePaidTo)
import qualified Plutus.Script.Utils.V2.Scripts       as PSU.V2
import qualified Plutus.Script.Utils.V2.Typed.Scripts as PlutusV2
import qualified Plutus.Script.Utils.Value            as Value
import           Plutus.V1.Ledger.Address             (pubKeyHashAddress)
import           Plutus.V1.Ledger.Value               (assetClassValueOf)
import qualified Plutus.V2.Ledger.Api                 as PlutusV2
import           Plutus.V2.Ledger.Contexts            (txSignedBy)
import qualified Plutus.V2.Ledger.Contexts            as PlutusV2
import qualified PlutusTx
import           PlutusTx.Prelude                     as P hiding
                                                            (Semigroup (..),
                                                            unless, (.))
import           Prelude                              (FilePath, IO, Show (..),
                                                       print, putStrLn, (.))

{-
There are four cases will happen on market place:
+ BUY: user will make a request to buy the asset on our system, then the user
will interact with the Marketplace Contract to buy and update asset's owner.
+ WITHDRAW: user will make a request to withdraw the asset, then the user
will interact with the Marketplace Contract to withdraw.
+ CANCELL: the user pending trade on marketplace
+ SELL: the user change new price for asset
+ BUY_PARTIAL: the user can buy a part of the order
-}
data RedeemerParams
  = BUY
  | WITHDRAW
  | SELL
      { newPrice :: Integer
      }
  | CANCELL
  | BUY_PARTIAL
      { amt :: Integer
      }
  deriving (Show)

PlutusTx.makeLift ''RedeemerParams

PlutusTx.makeIsDataIndexed
  ''RedeemerParams
  [('BUY, 0), ('WITHDRAW, 1), ('SELL, 2), ('CANCELL, 3), ('BUY_PARTIAL, 4)]

-- This is the validator function of Marketplace Contract
{-# INLINABLE mkValidator #-}
mkValidator ::
     () -> AssetDatumParams -> RedeemerParams -> PlutusV2.ScriptContext -> Bool
mkValidator _ dParams rParams scriptContext =
  traceIfFalse
    "[Plutus Error]: cannot find the asset in input"
    (not (Value.isAdaOnlyValue theAssetInInput)) &&
  case rParams of
    BUY ->
      traceIfFalse "[Plutus Error]: this asset is not enough" checkAssetInInput &&
      traceIfFalse
        "[Plutus Error]: this asset is not for sale"
        (salePrice dParams > 0) &&
      traceIfFalse
        "[Plutus Error]: the asset in output must be sent to the Marketplace Contract address only"
        checkAssetInOutput &&
      traceIfFalse
        "[Plutus Error]: output datum (buy) is not correct"
        (checkBuyerTxOutHasAsset (assetAmount dParams)) &&
      traceIfFalse
        "[Plutus Error]: Seller not paid"
        (assetClassValueOf
           (valuePaidTo info (pubKeyHashAddress (owner dParams)))
           adaAsset >=
         salePrice dParams * assetAmount dParams)
    BUY_PARTIAL amountBuy ->
      traceIfFalse "[Plutus Error]: this asset is not enough" checkAssetInInput &&
      traceIfFalse
        "[Plutus Error]: this asset is not for sale"
        (salePrice dParams > 0) &&
      traceIfFalse
        "[Plutus Error]: amount for buy has to smaller total"
        (amountBuy <= assetAmount dParams) &&
      traceIfFalse
        "[Plutus Error]: the asset in output must be sent to the Marketplace Contract address only"
        checkAssetInOutput &&
      traceIfFalse
        "[Plutus Error]: output datum (buy) is not correct"
        (if amountBuy == assetAmount dParams
           then checkBuyerTxOutHasAsset amountBuy
           else checkBuyerTxOutHasAsset amountBuy &&
                checkChangeTxOutHasAsset (assetAmount dParams - amountBuy)) &&
      traceIfFalse
        "[Plutus Error]: Seller not paid"
        (assetClassValueOf
           (valuePaidTo info (pubKeyHashAddress (owner dParams)))
           adaAsset >=
         salePrice dParams * amountBuy)
    SELL newPrice ->
      traceIfFalse "[Plutus Error]: this asset is not enough" checkAssetInInput &&
      traceIfFalse
        "[Plutus Error]: owner is not correct"
        (txSignedBy info $ owner dParams) &&
      traceIfFalse
        "[Plutus Error]: the asset in output must be sent to the Marketplace Contract address only"
        checkAssetInOutput &&
      traceIfFalse
        "[Plutus Error]: output datum is not correct"
        (checkOutputDatumSell newPrice $
         parseOutputDatumInTxOut getTxOutHasAsset)
    CANCELL ->
      traceIfFalse "[Plutus Error]: this asset is not enough" checkAssetInInput &&
      traceIfFalse
        "[Plutus Error]: owner is not correct"
        (txSignedBy info $ owner dParams) &&
      traceIfFalse
        "[Plutus Error]: the asset in output must be sent to the Marketplace Contract address only"
        checkAssetInOutput &&
      traceIfFalse
        "[Plutus Error]: output datum is not correct"
        (checkOutputDatumCancell $ parseOutputDatumInTxOut getTxOutHasAsset)
    WITHDRAW ->
      traceIfFalse
        "[Plutus Error]: owner is not correct"
        (txSignedBy info $ owner dParams)
    _ -> False
    -- Get all info about the transaction
  where
    info :: PlutusV2.TxInfo
    info = PlutusV2.scriptContextTxInfo scriptContext
    adaAsset :: Value.AssetClass
    adaAsset = Value.AssetClass (PlutusV2.adaSymbol, PlutusV2.adaToken)
    -- Get all script outputs
    allTxOut :: [PlutusV2.TxOut]
    allTxOut = PlutusV2.getContinuingOutputs scriptContext
    mainInput :: PlutusV2.TxOut
    mainInput =
      case PlutusV2.findOwnInput scriptContext of
        Nothing ->
          traceError
            "[Plutus Error]: cannot find the input associated with the Marketplace Contract address"
        Just i -> PlutusV2.txInInfoResolved i
    -- Get the Marketplace Contract address associated with the asset in input
    -- contractAddress :: PlutusV2.Address
    -- contractAddress = PlutusV2.txOutAddress mainInput
    -- Get the value of the Asset
    theAssetInInput :: PlutusV2.Value
    theAssetInInput = PlutusV2.txOutValue mainInput
    checkAssetInInput :: Bool
    checkAssetInInput =
      let (currencySymbol, tokenName) = Value.unAssetClass (assetClass dParams)
          val = Value.currencyValueOf theAssetInInput currencySymbol
          [(_, tn, amt)] = Value.flattenValue val
       in amt == assetAmount dParams && tokenName == tn
    {-
    This function is to check which address that the asset has been sent to in outputs
    In case of buying and reselling, the asset must be sent to the Marketplace Contract address only.
    -}
    checkAssetInOutput :: Bool
    checkAssetInOutput =
      case find
             (\x ->
                Value.symbols (PlutusV2.txOutValue x) ==
                Value.symbols theAssetInInput)
             allTxOut of
        Nothing -> False
        Just _  -> True
    {-
    This function will check whether the asset is in the outputs or not.
    If yes, the txout will be used to parse and check whether the output datum is correct or not.
    -}
    getTxOutHasAsset :: PlutusV2.TxOut
    getTxOutHasAsset =
      case find
             (\x ->
                Value.symbols (PlutusV2.txOutValue x) ==
                Value.symbols theAssetInInput)
             allTxOut of
        Nothing -> traceError "[Plutus Error]: cannot find the asset in output"
        Just i -> i
    checkBuyerTxOutHasAsset :: Integer -> Bool
    checkBuyerTxOutHasAsset amount =
      case find
             (\x ->
                Value.symbols (PlutusV2.txOutValue x) ==
                Value.symbols theAssetInInput &&
                checkOutputDatumBuyPartial amount (parseOutputDatumInTxOut x))
             allTxOut of
        Nothing ->
          traceIfFalse "[Plutus Error]: cannot find the asset in output" False
        Just _ -> True
    checkChangeTxOutHasAsset :: Integer -> Bool
    checkChangeTxOutHasAsset amount =
      case find
             (\x ->
                Value.symbols (PlutusV2.txOutValue x) ==
                Value.symbols theAssetInInput &&
                checkOutputDatumBuyChangePartial
                  amount
                  (parseOutputDatumInTxOut x))
             allTxOut of
        Nothing ->
          traceIfFalse "[Plutus Error]: cannot find the asset in output" False
        Just _ -> True
    -- Parse output datum to the AssetDatumParams format
    parseOutputDatumInTxOut :: PlutusV2.TxOut -> Maybe AssetDatumParams
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
    checkOutputDatumBuyPartial :: Integer -> Maybe AssetDatumParams -> Bool
    checkOutputDatumBuyPartial amountBuy outputDatum =
      case outputDatum of
        Just (AssetDatumParams newOwner salePrice' ast asamt) ->
          traceIfFalse
            "[Plutus Error]: new owner must not be empty and must be different with the old one"
            (PlutusV2.getPubKeyHash newOwner /= "" && newOwner /= owner dParams) &&
          traceIfFalse
            "[Plutus Error]: asset info must not change"
            (ast == assetClass dParams && asamt == amountBuy) &&
          traceIfFalse
            "[Plutus Error]: sale price has to equal zero"
            (salePrice' == 0)
        Nothing -> traceError "[Plutus Error]: output datum must not be empty"
      -- Check output datum in case of buying the asset on market place
    checkOutputDatumBuyChangePartial ::
         Integer -> Maybe AssetDatumParams -> Bool
    checkOutputDatumBuyChangePartial amountBuy outputDatum =
      case outputDatum of
        Just (AssetDatumParams oldOwner salePrice' ast asamt) ->
          traceIfFalse
            "[Plutus Error]: new owner must not be empty and must be different with the old one"
            (oldOwner == owner dParams) &&
          traceIfFalse
            "[Plutus Error]: asset info must not change"
            (ast == assetClass dParams && asamt == amountBuy) &&
          traceIfFalse
            "[Plutus Error]: sale price has to no change"
            (salePrice' == salePrice dParams)
        Nothing -> traceError "[Plutus Error]: output datum must not be empty"
    -- Check output datum in case of sell the asset on market place
    checkOutputDatumSell :: Integer -> Maybe AssetDatumParams -> Bool
    checkOutputDatumSell newPrice outputDatum =
      case outputDatum of
        Just (AssetDatumParams owner' newPrice' ast asamt) ->
          traceIfFalse
            "[Plutus Error]: owner must not be the same"
            (owner' == owner dParams) &&
          traceIfFalse
            "[Plutus Error]: this asset is not for sale"
            (newPrice == newPrice' && newPrice > 0) &&
          traceIfFalse
            "[Plutus Error]: asset info must not change"
            (ast == assetClass dParams && asamt == assetAmount dParams)
        Nothing -> traceError "[Plutus Error]: output datum must not be empty"
    -- Check output datum in case of sell the asset on market place
    checkOutputDatumCancell :: Maybe AssetDatumParams -> Bool
    checkOutputDatumCancell outputDatum =
      case outputDatum of
        Just (AssetDatumParams owner' newPrice ast asamt) ->
          traceIfFalse
            "[Plutus Error]: owner must not be the same"
            (owner' == owner dParams) &&
          traceIfFalse
            "[Plutus Error]: this asset is not for sale"
            (newPrice == 0) &&
          traceIfFalse
            "[Plutus Error]: asset info must not change"
            (ast == assetClass dParams && asamt == assetAmount dParams)
        Nothing -> traceError "[Plutus Error]: output datum must not be empty"

data ContractType

instance Scripts.ValidatorTypes ContractType where
  type DatumType ContractType = AssetDatumParams
  type RedeemerType ContractType = RedeemerParams

typedMarketplaceValidator :: () -> PlutusV2.TypedValidator ContractType
typedMarketplaceValidator = PlutusV2.mkTypedValidatorParam @ContractType
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.mkUntypedValidator

validator :: () -> PSU.V2.Validator
validator = Scripts.validatorScript . typedMarketplaceValidator

validatorHash :: () -> PSU.V2.ValidatorHash
validatorHash = Scripts.validatorHash . typedMarketplaceValidator

script :: () -> PlutusV2.Script
script = PlutusV2.unValidatorScript . validator

scriptSBS :: () -> SBS.ShortByteString
scriptSBS = SBS.toShort . LBS.toStrict . serialise . script

buildMarketplaceContract :: () -> PlutusScript PlutusScriptV2
buildMarketplaceContract = PlutusScriptSerialised . scriptSBS

---------------------------------------------------------------------------------------------------------------
------------------------------------------------- HELPER FUNCTIONS --------------------------------------------
-- This is another version for marketplace contract, it uses dynamic params to build the parameterized contract
-- We will apply it later
{-# INLINABLE wrapValidator #-}
wrapValidator ::
     (PlutusTx.UnsafeFromData a)
  => (a -> RedeemerParams -> PlutusV2.ScriptContext -> Bool) -- ^
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

saveMarketplaceCode :: IO ()
saveMarketplaceCode =
  writeCodeToFile
    "./built-contracts/marketplace-parameterized-contract.json"
    validatorCode
