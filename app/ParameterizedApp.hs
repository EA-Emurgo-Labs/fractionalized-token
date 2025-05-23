{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

import           FNFTContract    (saveFNFTCode)
import           MintingContract (saveNFTMintingCode)
import           Prelude         (IO)

-- This is the main function for parameterized contracts
main :: IO ()
main = do
    FNFTContract.saveFNFTCode
    MintingContract.saveNFTMintingCode

