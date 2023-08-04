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
    FNFTDatum(..)
    , fractionTokenName
    , validityTokenName
  )
where

import qualified Plutus.Script.Utils.Value as Value
import qualified PlutusTx
import           PlutusTx.Prelude          as P hiding (Semigroup (..), unless,
                                                 (.))
import           Prelude                   (Show (..))

{-
This is the datum attached with the NFT when it goes through every phase in the system.
Currently, we will manage the number of transfers, current price, max price, NFT's owner
and the sale price (when user resell the NFT on market place).
-}

data FNFTDatum = FNFTDatum
  { fractionAC       :: !Value.AssetClass,
    emittedFractions :: !Integer
  }
  deriving (Show)

PlutusTx.makeLift ''FNFTDatum
PlutusTx.makeIsDataIndexed ''FNFTDatum [('FNFTDatum,0)]

fractionTokenName = Value.TokenName "ADA NFT A FRACTION"

validityTokenName = Value.TokenName "FNFT_VALIDITY"
