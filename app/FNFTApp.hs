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
import           FNFTContract
import           PlutusTx.Prelude    as P hiding (Semigroup (..), unless, (.))
import           Prelude             (IO)
import qualified Prelude             as Haskell

main :: IO ()
main = do
  let fnftContract = "built-contracts/fnft-contract.json"

  -- Built the plutus script for Marketplace Contract
  resultFNFTContract <- writeFileTextEnvelope fnftContract Nothing $ buildFNFTContract ()
  case resultFNFTContract of
    Left err -> Haskell.print $ displayError err
    Right () -> Haskell.putStrLn $ "Built marketplace contract successfully at: " ++ fnftContract
