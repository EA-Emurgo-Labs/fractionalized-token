{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Utility
  ( toCurrencySymbol
  , toValidatorHash
  ) where

import qualified Data.Aeson.Extras as JSON
import qualified Data.Text as T
import qualified Plutus.Script.Utils.Value as Value
import qualified Plutus.V2.Ledger.Api as PlutusV2
import PlutusTx.Prelude as P
  ( Either(Left, Right)
  , ($)
  , emptyByteString
  , fromBuiltin
  , toBuiltin
  )
import Prelude ((.))
import qualified Prelude as Haskell

-- This function is to convert from string to currency symbol
toCurrencySymbol :: Haskell.String -> PlutusV2.CurrencySymbol
toCurrencySymbol str =
  case (JSON.tryDecode . T.pack) str of
    Left _ -> (Value.currencySymbol . fromBuiltin) emptyByteString
    Right b -> Value.currencySymbol b

-- This function is to convert from string to validatior hash (of contract address)
toValidatorHash :: Haskell.String -> PlutusV2.ValidatorHash
toValidatorHash str =
  case JSON.tryDecode $ T.pack str of
    Left _ -> PlutusV2.ValidatorHash emptyByteString
    Right b -> PlutusV2.ValidatorHash $ toBuiltin b
