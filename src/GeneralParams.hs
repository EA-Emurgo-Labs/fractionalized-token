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
  (
    AssetDatumParams(..)
  )
where

import           Plutus.Script.Utils.Value (AssetClass)
import qualified Plutus.V2.Ledger.Api      as PlutusV2
import qualified PlutusTx
import           PlutusTx.Prelude          as P hiding (Semigroup (..), unless,
                                                 (.))
import           Prelude                   (Show (..))

{-
This is the datum attached with the NFT when it goes through every phase in the system.
Currently, we will manage the number of transfers, current price, max price, NFT's owner
and the sale price (when user resell the NFT on market place).
-}
data AssetDatumParams = AssetDatumParams
  {
    owner       :: PlutusV2.PubKeyHash,
    salePrice   :: Integer,
    assetClass  :: AssetClass,
    assetAmount :: Integer
  }
  deriving(Show)

PlutusTx.makeLift ''AssetDatumParams
PlutusTx.makeIsDataIndexed ''AssetDatumParams [('AssetDatumParams,0)]
