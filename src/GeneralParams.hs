{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
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

module GeneralParams
  ( FNFTDatum(..)
  , validityTokenName
  , MintingRedeemer(..)
  , FNFTRedeemer(..)
  ) where

import           Data.ByteString           (ByteString)
import qualified Data.ByteString           as Data
import           Plutus.Script.Utils.Value (TokenName)
import qualified Plutus.Script.Utils.Value as Value
import           Plutus.V2.Ledger.Tx       (TxId, TxOutRef)
import qualified PlutusTx
import           PlutusTx.Prelude          as P (BuiltinByteString, Integer)
import           Prelude                   (Show (..))

data FNFTDatum =
  FNFTDatum
    { fractionCS        :: Value.CurrencySymbol
    , fractionTN        :: Value.TokenName
    , emittedFractions  :: !Integer
    , nftCS             :: Value.CurrencySymbol
    , nftTN             :: Value.TokenName
    , remainedFractions :: !Integer
    }
  deriving (Show)

PlutusTx.makeLift ''FNFTDatum
PlutusTx.makeIsDataIndexed ''FNFTDatum [('FNFTDatum, 0)]

validityTokenName :: Value.TokenName
validityTokenName = Value.TokenName "FNFT_VALIDITY"

data MintingRedeemer
  = InitialMint BuiltinByteString Integer
  | Burn
  deriving (Show)

PlutusTx.makeLift ''MintingRedeemer
PlutusTx.makeIsDataIndexed ''MintingRedeemer [('InitialMint, 0), ('Burn, 1)]

data FNFTRedeemer
  = Withdraw Integer
  | Claim
  | Deposit Integer
  deriving (Show)

PlutusTx.makeLift ''FNFTRedeemer
PlutusTx.makeIsDataIndexed ''FNFTRedeemer [('Withdraw, 0), ('Claim, 1), ('Deposit, 2)]

