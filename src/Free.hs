{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Free (
    freePolicy,
    freeCurrencySymbol,
) where

import           Ledger.Typed.Scripts           as Scripts (IsScriptContext (mkUntypedMintingPolicy),
                                                            MintingPolicy)
import           Plutus.Script.Utils.V2.Scripts (scriptCurrencySymbol)
import           Plutus.V2.Ledger.Api           (CurrencySymbol, ScriptContext,
                                                 mkMintingPolicyScript)
import qualified PlutusTx
import           PlutusTx.Prelude               (Bool (True))

{-# INLINABLE mkFreePolicy #-}
mkFreePolicy :: () -> Plutus.V2.Ledger.Api.ScriptContext -> Bool
mkFreePolicy () _ = True

-- {-# INLINABLE mkWrappedFreePolicy #-}
-- mkWrappedFreePolicy :: BuiltinData -> BuiltinData -> ()
-- mkWrappedFreePolicy = wrapPolicy mkFreePolicy

freePolicy :: MintingPolicy
freePolicy = Plutus.V2.Ledger.Api.mkMintingPolicyScript $$(PlutusTx.compile[|| Scripts.mkUntypedMintingPolicy (mkFreePolicy) ||])

---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------

-- saveFreePolicy :: IO ()
-- saveFreePolicy = writePolicyToFile "assets/free.plutus" freePolicy

freeCurrencySymbol :: CurrencySymbol
freeCurrencySymbol = scriptCurrencySymbol freePolicy
