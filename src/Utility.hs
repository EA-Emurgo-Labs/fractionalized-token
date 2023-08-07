{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Utility
  ( toCurrencySymbol
  , toValidatorHash
  , extractMintedAmt
  , extractMintedTokens
  , calculateFractionTokenNameHash
  , hasUTxO
  , getInput
  , parseOutputDatumInTxOut
  ) where

import qualified Data.Aeson.Extras               as JSON
import qualified Data.Text                       as T
import           GeneralParams                   (FNFTDatum)
import           Plutus.Script.Utils.V2.Contexts (findDatum)
import qualified Plutus.Script.Utils.Value       as Value
import qualified Plutus.V2.Ledger.Api            as PlutusV2
import           PlutusTx.Prelude                as P (Bool (False, True),
                                                       Either (Left, Right),
                                                       Eq ((==)), Integer,
                                                       Maybe (..), any,
                                                       consByteString,
                                                       emptyByteString, find,
                                                       fromBuiltin, sha2_256,
                                                       snd, toBuiltin, ($),
                                                       (&&), (.))
import qualified Prelude                         as Haskell

-- This function is to convert from string to currency symbol
toCurrencySymbol :: Haskell.String -> PlutusV2.CurrencySymbol
toCurrencySymbol str =
  case (JSON.tryDecode . T.pack) str of
    Left _  -> (Value.currencySymbol . fromBuiltin) emptyByteString
    Right b -> Value.currencySymbol b

-- This function is to convert from string to validatior hash (of contract address)
toValidatorHash :: Haskell.String -> PlutusV2.ValidatorHash
toValidatorHash str =
  case JSON.tryDecode $ T.pack str of
    Left _  -> PlutusV2.ValidatorHash emptyByteString
    Right b -> PlutusV2.ValidatorHash $ toBuiltin b

{-# INLINEABLE extractMintedAmt #-}
extractMintedAmt ::
     PlutusV2.TokenName -> [(PlutusV2.TokenName, Integer)] -> Integer
extractMintedAmt exTokenName mintedTokens =
  let token =
        find (\(exTokenName', _) -> exTokenName' == exTokenName) mintedTokens
   in case token of
        Just a -> snd a
        _      -> 0

{-# INLINEABLE extractMintedTokens #-}
extractMintedTokens ::
     PlutusV2.CurrencySymbol
  -> PlutusV2.Value
  -> [(PlutusV2.TokenName, Integer)]
extractMintedTokens mintedSymbol txMint =
  [(tn, amt) | (cs, tn, amt) <- Value.flattenValue txMint, cs == mintedSymbol]

{-# INLINABLE calculateFractionTokenNameHash #-}
calculateFractionTokenNameHash ::
     PlutusV2.TxOutRef -> PlutusV2.BuiltinByteString
calculateFractionTokenNameHash utxo =
  sha2_256
    (consByteString
       (PlutusV2.txOutRefIdx utxo)
       ((PlutusV2.getTxId . PlutusV2.txOutRefId) utxo))

{-# INLINABLE hasUTxO #-}
hasUTxO :: PlutusV2.TxOutRef -> PlutusV2.TxInfo -> Bool
hasUTxO utxo info =
  any (\i -> PlutusV2.txInInfoOutRef i == utxo) $ PlutusV2.txInfoInputs info

{-# INLINABLE getInput #-}
getInput ::
     PlutusV2.CurrencySymbol -> [PlutusV2.TxInInfo] -> Maybe PlutusV2.TxInInfo
getInput cs' =
  find
    (\a -> do
       let value' = PlutusV2.txOutValue (PlutusV2.txInInfoResolved a)
           flatValues = Value.flattenValue value'
       case find (\(cs, tn, amt) -> cs == cs' && amt == 1) flatValues of
         Nothing -> False
         Just _  -> True)

parseOutputDatumInTxOut :: PlutusV2.TxInfo -> PlutusV2.TxOut -> Maybe FNFTDatum
parseOutputDatumInTxOut info txout =
      case PlutusV2.txOutDatum txout of
        PlutusV2.NoOutputDatum -> Nothing
        PlutusV2.OutputDatum od ->
          PlutusV2.fromBuiltinData $ PlutusV2.getDatum od
        PlutusV2.OutputDatumHash odh ->
          case findDatum odh info of
            Just od -> PlutusV2.fromBuiltinData $ PlutusV2.getDatum od
            Nothing -> Nothing
