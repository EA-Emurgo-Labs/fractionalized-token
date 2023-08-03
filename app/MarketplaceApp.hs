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

import           Cardano.Api         hiding (TxId)
import           MarketplaceContract
import           PlutusTx.Prelude    as P hiding (Semigroup (..), unless, (.))
import           Prelude             (IO)
import qualified Prelude             as Haskell

main :: IO ()
main = do
  let marketplaceContract = "built-contracts/marketplace-contract.json"

  -- Built the plutus script for Marketplace Contract
  resultMarketplaceContract <- writeFileTextEnvelope marketplaceContract Nothing $ buildMarketplaceContract ()
  case resultMarketplaceContract of
    Left err -> Haskell.print $ displayError err
    Right () -> Haskell.putStrLn $ "Built marketplace contract successfully at: " ++ marketplaceContract
