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
import Plutus.Script.Utils.Value (TokenName(TokenName))

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
    Burn -> validateBurn scriptContext

{-# INLINEABLE extractMintedAmt #-}
extractMintedAmt :: PlutusV2.TokenName -> [(PlutusV2.TokenName, Integer)] -> Integer
extractMintedAmt exTokenName mintedTokens =
  let token = find (\(exTokenName', _) -> exTokenName' == exTokenName) mintedTokens
  in case token of
    Just a -> snd a
    _ -> 0

{-# INLINEABLE extractMintedTokens #-}
extractMintedTokens :: PlutusV2.CurrencySymbol -> PlutusV2.Value -> [(PlutusV2.TokenName, Integer)]
extractMintedTokens mintedSymbol txMint =
  [(tn, amt) | (cs, tn, amt) <- Value.flattenValue txMint, cs == mintedSymbol]

fractionTokenName = TokenName "ADA NFT A FRACTION"

{-# INLINEABLE validateInitialMint #-}
validateInitialMint :: TxOutRef -> ScriptContext -> Bool
validateInitialMint utxo ctx = traceIfFalse "Minted ammount fractions not positive" (fractionTokensMintedAmount > 0)
  where
    info :: PlutusV2.TxInfo
    info = PlutusV2.scriptContextTxInfo ctx
    txMint = txInfoMint info
    ownCS = ownCurrencySymbol ctx
    extractedMintedTokens = extractMintedTokens ownCS txMint
    fractionTokensMintedAmount = extractMintedAmt fractionTokenName extractedMintedTokens

validateBurn :: ScriptContext -> Bool
validateBurn ctx = True

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
