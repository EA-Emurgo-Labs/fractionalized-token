{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE NoImplicitPrelude  #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main where

import           Data.ByteString           (count)
import qualified Data.ByteString.Char8     as BS8
import           Data.Maybe                (fromJust)
import           Data.String
import           GeneralParams
import qualified GeneralParams
import           GHC.Num                   ((*))
import           Plutus.Model
import           Plutus.Script.Utils.Value
import           Plutus.V2.Ledger.Api
import           PlutusTx.Builtins
import           PlutusTx.Prelude          (Bool (..), Eq ((==)), Ord ((>)),
                                            find, isJust, length, return, ($),
                                            (-), (.))
import           Prelude                   (Bool (True), IO,
                                            Maybe (Just, Nothing),
                                            Monad (return), Semigroup ((<>)),
                                            head, last, mconcat, not, pure,
                                            tail, (&&), (<$>))
import           System.IO                 (IO)
import           Test.QuickCheck           (Arbitrary (arbitrary), Property,
                                            Testable (property), choose, (==>))
import           Test.QuickCheck.Monadic   (assert, monadic, run)
import           Test.Tasty                (defaultMain, testGroup)
import           Test.Tasty.QuickCheck     as QC (testProperty)
import           Wallet                    (payToPaymentPublicKeyHash)

---------------------------------------------------------------------------------------------------
--------------------------------------- TESTING MAIN ----------------------------------------------

-- | Test the validator script
main :: IO ()
main = defaultMain $
    testGroup
  "Testing script properties"
  [
    testGroup "Fractionalized NFT contract"
      [ testProperty "Check mint fnft " prop_mint_fnft
      ]
  ]

-- ---------------------------------------------------------------------------------------------------
-- -------------------------------- HELPER FUNCTIONS/INSTANCES ---------------------------------------

-- Init a new blockchain with 10,000,000 ADA and 1 Emurgo token (the operator token).
mock :: Mock
mock = initMock defaultBabbage (adaValue 10_000_000 <> emurgoValue)

instance Testable a => Testable (Run a) where
  property rp = let (a,_) = runMock rp mock in property a

-- Convert from String to BuiltinByteString
toBuiltinByteString :: String -> BuiltinByteString
toBuiltinByteString = fromString

-- Create a fake coin, it will be used to create the operator token.
fake :: FakeCoin
fake = FakeCoin $ toBuiltinByteString "Emurgo"

-- Create the operator token, this token is used to verify who is the operator, only the operator
-- can run the Minting Contract and Marketplace Contract.
emurgoToken :: AssetClass
emurgoToken = fakeCoin fake

-- Init the value for operator token (just one).
emurgoValue :: Value
emurgoValue = fakeValue fake 1000

-- Set up users, including: one operator and one normal user.
setupUsers :: Run [PubKeyHash]
setupUsers = do
  seller    <- newUser (adaValue 1000 <> emurgoValue)
  buyer <- newUser (adaValue 1000)
  pure [seller, buyer]

-- -- Create the marketpalce contract.
-- marketplaceContract :: TypedValidator datum redeemer
-- marketplaceContract = TypedValidator $ toV2 $ Marketplace.validator ()

-- ---------------------------------------------------------------------------------------------------
-- ------------------------------------- TESTING PROPERTIES ------------------------------------------

prop_mint_fnft :: Property
prop_mint_fnft = runChecks

-- prop_withdraw_asset :: Property
-- prop_withdraw_asset = runChecks 0 Marketplace.WITHDRAW True

-- prop_withdraw_asset_fail :: Property
-- prop_withdraw_asset_fail = runChecks 0 Marketplace.WITHDRAW False

-- prop_cancel_asset :: Property
-- prop_cancel_asset = runChecks 0 Marketplace.CANCELL True

-- prop_cancel_asset_fail :: Property
-- prop_cancel_asset_fail = runChecks 0 Marketplace.CANCELL False

-- prop_sell_asset :: Integer -> Property
-- prop_sell_asset newPrice'' = (newPrice'' > 0) ==> runChecks newPrice'' (Marketplace.SELL newPrice'') True

-- prop_sell_asset_fail :: Integer -> Property
-- prop_sell_asset_fail newPrice'' = (newPrice'' > 0) ==> runChecks newPrice'' (Marketplace.SELL newPrice'') False


-- ---------------------------------------------------------------------------------------------------
-- ------------------------------------- RUNNING THE TESTS -------------------------------------------
-- sellTx :: UserSpend -> Value -> GeneralParams.AssetDatumParams -> Tx
-- sellTx usp valNFT datum = mconcat
--   [ userSpend usp
--   , payToScript marketplaceContract (HashDatum datum) valNFT
--   ]

-- buyTx :: TxOutRef -> UserSpend ->  Marketplace.RedeemerParams -> GeneralParams.AssetDatumParams -> GeneralParams.AssetDatumParams -> PubKeyHash -> Value -> Value -> Tx
-- buyTx ref usp redeem oldDatum newDatum theOwner sPrice asset = mconcat
--   [ userSpend usp
--   , spendScript marketplaceContract ref redeem oldDatum
--   ,  payToScript marketplaceContract (HashDatum newDatum) asset
--   , payToKey theOwner sPrice
--   ]

-- buyPartialTx :: TxOutRef -> UserSpend ->  Marketplace.RedeemerParams -> GeneralParams.AssetDatumParams -> GeneralParams.AssetDatumParams -> GeneralParams.AssetDatumParams -> PubKeyHash -> Value -> Value -> Tx
-- buyPartialTx ref usp redeem oldDatum newDatum newDatum2 theOwner sPrice asset = mconcat
--   [ userSpend usp
--   , spendScript marketplaceContract ref redeem oldDatum
--   ,  payToScript marketplaceContract (HashDatum newDatum) asset
--   ,  payToScript marketplaceContract (HashDatum newDatum2) asset
--   , payToKey theOwner sPrice
--   ]

-- withdrawTx :: TxOutRef ->  Marketplace.RedeemerParams -> GeneralParams.AssetDatumParams -> PubKeyHash -> Value -> Tx
-- withdrawTx ref redeem oldDatum theOwner asset = mconcat
--   [spendScript marketplaceContract ref redeem oldDatum
--   , payToKey theOwner asset
--   ]

-- cancelTx :: TxOutRef ->  Marketplace.RedeemerParams -> GeneralParams.AssetDatumParams -> GeneralParams.AssetDatumParams -> Value -> Tx
-- cancelTx ref redeem oldDatum newDatum asset = mconcat
--   [
--     spendScript marketplaceContract ref redeem oldDatum
--   ,  payToScript marketplaceContract (HashDatum newDatum) asset
--   ]

runChecks :: Property
runChecks =
  monadic property check
  -- monadic property check
    where
      check = do
        isGood <- run $ testValues
        assert isGood

testValues :: Run Bool
testValues = do
    [seller, buyer] <- setupUsers
    noErrors
--   case redeem of
--     Marketplace.BUY -> do
--       let adaVal   = adaValue 100
--           nftVal   = fakeValue fake 10

--       uspSeller <- spend seller nftVal

--       let oldNftDatum = GeneralParams.AssetDatumParams {
--         GeneralParams.salePrice = sPrice,
--         GeneralParams.owner = seller,
--         GeneralParams.assetClass = emurgoToken,
--         GeneralParams.assetAmount = 10
--       }

--       let tx  = sellTx uspSeller nftVal oldNftDatum
--       submitTx seller tx
--       Plutus.Model.waitUntil 50

--       let newNftDatum = GeneralParams.AssetDatumParams {
--         GeneralParams.salePrice = 0,
--         GeneralParams.owner = buyer,
--         GeneralParams.assetClass = emurgoToken,
--         GeneralParams.assetAmount = 10
--       }

--       utxos <- utxoAt marketplaceContract

--       let (cs', tn') = unAssetClass emurgoToken
--       let nft = find(\x -> do
--                 let (_', txOut') = x
--                     value' = txOutValue txOut'
--                     [(cs, tn, amt)] = flattenValue value'
--                 cs' == cs && tn == tn' && amt == 10
--               ) utxos

--       let (txOutRef, _) = fromJust nft
--       let redeemParam = redeem
--       uspBuyer <- spend seller adaVal

--       let tx2 = buyTx txOutRef uspBuyer redeemParam oldNftDatum newNftDatum seller adaVal nftVal
--       submitTx buyer tx2
--       Plutus.Model.waitUntil 50

--       noErrors
--     Marketplace.BUY_PARTIAL amountPartial -> do
--       let adaVal   = adaValue 50
--           nftVal   = fakeValue fake 10
--           asset    = fakeValue fake 5

--       uspSeller <- spend seller nftVal

--       let oldNftDatum = GeneralParams.AssetDatumParams {
--         GeneralParams.salePrice = 10,
--         GeneralParams.owner = seller,
--         GeneralParams.assetClass = emurgoToken,
--         GeneralParams.assetAmount = 10
--       }

--       let tx  = sellTx uspSeller nftVal oldNftDatum
--       submitTx seller tx
--       Plutus.Model.waitUntil 50

--       let newNftDatum = GeneralParams.AssetDatumParams {
--         GeneralParams.salePrice = 0,
--         GeneralParams.owner = buyer,
--         GeneralParams.assetClass = emurgoToken,
--         GeneralParams.assetAmount = 5
--       }

--       let newNftDatum2 = GeneralParams.AssetDatumParams {
--         GeneralParams.salePrice = 10,
--         GeneralParams.owner = seller,
--         GeneralParams.assetClass = emurgoToken,
--         GeneralParams.assetAmount = 5
--       }

--       utxos <- utxoAt marketplaceContract

--       let (cs', tn') = unAssetClass emurgoToken
--       let nft = find(\x -> do
--                 let (_', txOut') = x
--                     value' = txOutValue txOut'
--                     [(cs, tn, amt)] = flattenValue value'
--                 cs' == cs && tn == tn' && amt == 10
--               ) utxos

--       let (txOutRef, _) = fromJust nft
--       let redeemParam = redeem
--       uspBuyer <- spend seller adaVal

--       let tx2 = buyPartialTx txOutRef uspBuyer redeemParam oldNftDatum newNftDatum newNftDatum2 seller adaVal asset
--       submitTx buyer tx2
--       Plutus.Model.waitUntil 50

--       noErrors

--     Marketplace.WITHDRAW -> do
--       let nftVal   = fakeValue fake 10

--       uspSeller <- spend seller nftVal

--       let oldNftDatum = GeneralParams.AssetDatumParams {
--         GeneralParams.salePrice = sPrice,
--         GeneralParams.owner = seller,
--         GeneralParams.assetClass = emurgoToken,
--         GeneralParams.assetAmount = 10
--       }

--       let tx  = sellTx uspSeller nftVal oldNftDatum
--       submitTx seller tx
--       Plutus.Model.waitUntil 50


--       utxos <- utxoAt marketplaceContract

--       let (cs', tn') = unAssetClass emurgoToken
--       let nft = find(\x -> do
--                 let (_', txOut') = x
--                     value' = txOutValue txOut'
--                     [(cs, tn, _)] = flattenValue value'
--                 cs' == cs && tn == tn'
--               ) utxos

--       let (txOutRef, _) = fromJust nft
--       let redeemParam = redeem

--       let tx2 = withdrawTx txOutRef redeemParam oldNftDatum seller nftVal
--       if isSeller then submitTx seller tx2 else mustFail $ submitTx buyer tx2
--       Plutus.Model.waitUntil 50
--       noErrors

--     Marketplace.CANCELL -> do
--       let nftVal   = fakeValue fake 10

--       uspSeller <- spend seller nftVal

--       let oldNftDatum = GeneralParams.AssetDatumParams {
--         GeneralParams.salePrice = 100,
--         GeneralParams.owner = seller,
--         GeneralParams.assetClass = emurgoToken,
--         GeneralParams.assetAmount = 10
--       }

--       let tx  = sellTx uspSeller nftVal oldNftDatum
--       submitTx seller tx
--       Plutus.Model.waitUntil 50

--       let newNftDatum = GeneralParams.AssetDatumParams {
--         GeneralParams.salePrice = 0,
--         GeneralParams.owner = seller,
--         GeneralParams.assetClass = emurgoToken,
--         GeneralParams.assetAmount = 10
--       }

--       utxos <- utxoAt marketplaceContract

--       let (cs', tn') = unAssetClass emurgoToken
--       let nft = find(\x -> do
--                 let (_', txOut') = x
--                     value' = txOutValue txOut'
--                     [(cs, tn, _)] = flattenValue value'
--                 cs' == cs && tn == tn'
--               ) utxos

--       let (txOutRef, _) = fromJust nft
--       let redeemParam = redeem
--       let tx2 = cancelTx txOutRef redeemParam oldNftDatum newNftDatum nftVal
--       if isSeller then submitTx seller tx2 else mustFail $ submitTx buyer tx2
--       Plutus.Model.waitUntil 50
--       noErrors
--     Marketplace.SELL newPrice' -> do
--       let nftVal   = fakeValue fake 10

--       uspSeller <- spend seller nftVal

--       let oldNftDatum = GeneralParams.AssetDatumParams {
--         GeneralParams.salePrice = 100,
--         GeneralParams.owner = seller,
--         GeneralParams.assetClass = emurgoToken,
--         GeneralParams.assetAmount = 10
--       }

--       let tx  = sellTx uspSeller nftVal oldNftDatum
--       submitTx seller tx
--       Plutus.Model.waitUntil 50

--       let newNftDatum = GeneralParams.AssetDatumParams {
--         GeneralParams.salePrice = newPrice',
--         GeneralParams.owner = seller,
--         GeneralParams.assetClass = emurgoToken,
--         GeneralParams.assetAmount = 10
--       }

--       utxos <- utxoAt marketplaceContract

--       let (cs', tn') = unAssetClass emurgoToken
--       let nft = find(\x -> do
--                 let (_', txOut') = x
--                     value' = txOutValue txOut'
--                     [(cs, tn, _)] = flattenValue value'
--                 cs' == cs && tn == tn'
--               ) utxos

--       let (txOutRef, _) = fromJust nft
--       let redeemParam = redeem
--       let tx2 = cancelTx txOutRef redeemParam oldNftDatum newNftDatum nftVal
--       if isSeller then submitTx seller tx2 else mustFail $ submitTx buyer tx2
--       Plutus.Model.waitUntil 50
--       noErrors
