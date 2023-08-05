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
import           FNFTContract
import           GeneralParams
import           GHC.Num                   ((*))
import           MintingContract           (MintingRedeemer (..))
import qualified MintingContract
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
  issuer    <- newUser (adaValue 1000 <> emurgoValue)
  buyer <- newUser (adaValue 1000)
  pure [issuer, buyer]

-- Create the marketpalce contract.
fnftContract :: TypedValidator datum redeemer
fnftContract = TypedValidator $ toV2 $ FNFTContract.validator ()

getMintingPolicy :: MintingPolicy
getMintingPolicy = MintingContract.policy ()

-- minting contract policy's script
policyMintingContractScript :: TypedPolicy MintingRedeemer
policyMintingContractScript = TypedPolicy $ toV2 Main.getMintingPolicy

-- ---------------------------------------------------------------------------------------------------
-- ------------------------------------- TESTING PROPERTIES ------------------------------------------

prop_mint_fnft :: Property
prop_mint_fnft = runChecks

-- ---------------------------------------------------------------------------------------------------
-- ------------------------------------- RUNNING THE TESTS -------------------------------------------
lockTx :: UserSpend -> TxOutRef -> Value -> Value -> Value -> GeneralParams.FNFTDatum -> Tx
lockTx usp ref valNFT fracVal validationVal datum = mconcat
  [ userSpend usp
  , mintValue policyMintingContractScript (InitialMint ref) (fracVal <> validationVal)
  , payToScript fnftContract (HashDatum datum) (valNFT <> fracVal <> validationVal)
  ]

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
    [issuer, buyer] <- setupUsers
    utxos <- utxoAt issuer
    let nftVal  = fakeValue fake 10
        fracTN = TokenName $ fromString "ADA NFT A FRACTION"
        validationTN = TokenName $ fromString "FNFT_VALIDITY"
        fracVal = singleton (MintingContract.mintingContractSymbol ()) fracTN 1000
        [(ref, out)] = utxos
        validationVal = singleton (MintingContract.mintingContractSymbol ()) validationTN 1

    uspIssuer <- spend issuer nftVal
    let fnftDatum = GeneralParams.FNFTDatum {
        GeneralParams.fractionAC = assetClass (MintingContract.mintingContractSymbol ()) fracTN
        , GeneralParams.emittedFractions = 1000
    }
    let tx  = lockTx uspIssuer ref nftVal fracVal  validationVal fnftDatum
    submitTx issuer tx
    Plutus.Model.waitUntil 50

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
