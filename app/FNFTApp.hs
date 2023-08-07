{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

import           Cardano.Api      (Error (displayError), writeFileTextEnvelope)
import           FNFTContract     (buildFNFTContract)
import           PlutusTx.Prelude as P (Either (Left, Right), Maybe (Nothing),
                                        ($), (++))
import           Prelude          (IO)
import qualified Prelude          as Haskell

main :: IO ()
main = do
  let fnftContract = "built-contracts/fnft-contract.json"
  -- Built the plutus script for Marketplace Contract
  resultFNFTContract <-
    writeFileTextEnvelope fnftContract Nothing $ buildFNFTContract ()
  case resultFNFTContract of
    Left err -> Haskell.print $ displayError err
    Right () ->
      Haskell.putStrLn $
      "Built marketplace contract successfully at: " ++ fnftContract
