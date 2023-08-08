{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

{-# HLINT ignore "Redundant bracket" #-}
module Main where

import           Data.ByteString           (count)
import qualified Data.ByteString.Char8     as BS8
import           Data.Maybe                (fromJust)
import           Data.String
import           FNFTContract
import           GeneralParams
import           GeneralParams             (FNFTDatum (nftAC, remainedFractions),
                                            FNFTRedeemer (..))
import           GHC.Num                   ((*))
import qualified MintingContract
import           Plutus.Model
import           Plutus.Script.Utils.Value
import           Plutus.V2.Ledger.Api
import           PlutusTx.Builtins
import           PlutusTx.Prelude          (Bool (..), Eq ((==)), Ord ((>)),
                                            find, isJust, isNothing, length,
                                            return, ($), (-), (.))
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
import           Utility
import           Wallet                    (payToPaymentPublicKeyHash)

---------------------------------------------------------------------------------------------------
--------------------------------------- TESTING MAIN ----------------------------------------------
-- | Test the validator script
main :: IO ()
main =
  defaultMain $
  testGroup
    "Testing script properties"
    [ testGroup
        "Fractionalized NFT contract"
        [ testProperty "Check full flow " prop_full_flow
        , testProperty "Check mint flow " prop_mint_flow
        , testProperty "Check withdraw flow " prop_withdraw_flow
        , testProperty "Check deposit flow " prop_deposit_flow
        , testProperty "Check burn flow " prop_burn_flow
        ]
    ]

-- ---------------------------------------------------------------------------------------------------
-- -------------------------------- HELPER FUNCTIONS/INSTANCES ---------------------------------------
-- Init a new blockchain with 10,000,000 ADA and 1 Emurgo token (the operator token).
mock :: Mock
mock = initMock defaultBabbage (adaValue 10_000_000 <> emurgoValue)

instance Testable a => Testable (Run a) where
  property rp =
    let (a, _) = runMock rp mock
     in property a

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
  issuer <- newUser (adaValue 1000 <> emurgoValue)
  buyer <- newUser (adaValue 1000)
  pure [issuer, buyer]

-- Create the marketpalce contract.
fnftContract :: TypedValidator datum redeemer
fnftContract = TypedValidator $ toV2 $ FNFTContract.validator ()

getMintingPolicy :: MintingPolicy
getMintingPolicy = MintingContract.policy $ FNFTContract.validatorHash ()

-- minting contract policy's script
policyMintingContractScript :: TypedPolicy MintingRedeemer
policyMintingContractScript = TypedPolicy $ toV2 Main.getMintingPolicy

-- ---------------------------------------------------------------------------------------------------
-- ------------------------------------- TESTING PROPERTIES ------------------------------------------
prop_full_flow :: Property
prop_full_flow = runChecks

prop_mint_flow :: Property
prop_mint_flow = runCheckMint

prop_withdraw_flow :: Property
prop_withdraw_flow = runCheckWithdraw

prop_deposit_flow :: Property
prop_deposit_flow = runCheckDeposit

prop_burn_flow :: Property
prop_burn_flow = runCheckBurn

-- ---------------------------------------------------------------------------------------------------
-- ------------------------------------- RUNNING THE TESTS -------------------------------------------
lockTx ::
     UserSpend
  -> TxOutRef
  -> Value
  -> Value
  -> Value
  -> GeneralParams.FNFTDatum
  -> Tx
lockTx usp ref valNFT fracVal validationVal datum =
  mconcat
    [ userSpend usp
    , mintValue
        policyMintingContractScript
        (InitialMint ref)
        (fracVal <> validationVal)
    , payToScript
        fnftContract
        (HashDatum datum)
        (valNFT <> fracVal <> validationVal)
    ]

unlockTx ::
     PubKeyHash
  -> TxOutRef
  -> Value
  -> Value
  -> Value
  -> GeneralParams.FNFTDatum
  -> Tx
unlockTx receiver ref valNFT fracVal validationVal datum =
  mconcat
    [ spendScript fnftContract ref Claim datum
    , mintValue policyMintingContractScript Burn (fracVal <> validationVal)
    , payToKey receiver (valNFT)
    ]

withdrawTx :: PubKeyHash
  -> TxOutRef
  -> Value
  -> Value
  -> Value
  -> Value
  -> GeneralParams.FNFTDatum
  -> GeneralParams.FNFTDatum
  -> Tx
withdrawTx receiver ref valNFT fracVal fracRemainedVal validationVal datumOld datumNew =
  mconcat
    [ spendScript fnftContract ref (Withdraw 10) datumOld
    , payToKey receiver (fracVal)
    , payToScript fnftContract  (HashDatum datumNew) (valNFT <> fracRemainedVal <> validationVal)
    ]

withdrawTx2 :: PubKeyHash
  -> TxOutRef
  -> Value
  -> Value
  -> Value
  -> Value
  -> GeneralParams.FNFTDatum
  -> GeneralParams.FNFTDatum
  -> Tx
withdrawTx2 receiver ref valNFT fracVal fracRemainedVal validationVal datumOld datumNew =
  mconcat
    [ spendScript fnftContract ref (Withdraw 10) datumOld
    , payToKey receiver (fracVal <> validationVal)
    , payToScript fnftContract  (HashDatum datumNew) (valNFT <> fracRemainedVal)
    ]

withdrawTx3 :: PubKeyHash
  -> TxOutRef
  -> Value
  -> Value
  -> Value
  -> Value
  -> GeneralParams.FNFTDatum
  -> GeneralParams.FNFTDatum
  -> Tx
withdrawTx3 receiver ref valNFT fracVal fracRemainedVal validationVal datumOld datumNew =
  mconcat
    [ spendScript fnftContract ref (Withdraw 10) datumOld
    , payToKey receiver (fracVal <> valNFT)
    , payToScript fnftContract  (HashDatum datumNew) (validationVal <> fracRemainedVal)
    ]

depositTx :: UserSpend
  -> TxOutRef
  -> Integer
  -> Value
  -> Value
  -> Value
  -> GeneralParams.FNFTDatum
  -> GeneralParams.FNFTDatum
  -> Tx
depositTx usp ref depositAmt valNFT fracRemainedVal validationVal datumOld datumNew =
  mconcat
    [ userSpend usp
    , spendScript fnftContract ref (Deposit depositAmt) datumOld
    , payToScript fnftContract  (HashDatum datumNew) (valNFT <> fracRemainedVal <> validationVal)
    ]

runCheckMint :: Property
runCheckMint = monadic property check
  where
    check = do
      isGood <- run $ testValueMint
      assert isGood

testValueMint :: Run Bool
testValueMint = do
  [issuer, user] <- setupUsers
  utxos <- utxoAt issuer
  let nftVal = fakeValue fake 1
      [(ref, out)] = utxos
      fracTN = TokenName $ calculateFractionTokenNameHash ref
      validationTN = TokenName $ fromString "FNFT_VALIDITY"
      fracVal = singleton (MintingContract.mintingContractSymbol $ FNFTContract.validatorHash ()) fracTN 1000
      fracVal1 = singleton (MintingContract.mintingContractSymbol $ FNFTContract.validatorHash ()) fracTN 1001
      validationVal =
        singleton (MintingContract.mintingContractSymbol $ FNFTContract.validatorHash ())  validationTN 1
  uspIssuer <- spend issuer nftVal
  let fnftDatum =
        GeneralParams.FNFTDatum
          { GeneralParams.fractionAC =
              assetClass (MintingContract.mintingContractSymbol $ FNFTContract.validatorHash ()) fracTN
          , GeneralParams.emittedFractions = 1000
          , GeneralParams.nftAC = emurgoToken
          , GeneralParams.remainedFractions = 1000
          }
  let fnftDatum2 =
        GeneralParams.FNFTDatum
          { GeneralParams.fractionAC =
              assetClass (MintingContract.mintingContractSymbol $ FNFTContract.validatorHash ()) validationTN
          , GeneralParams.emittedFractions = 1000
          , GeneralParams.nftAC = emurgoToken
          , GeneralParams.remainedFractions = 1000
          }
  let fnftDatum3 =
        GeneralParams.FNFTDatum
          { GeneralParams.fractionAC =
              assetClass (MintingContract.mintingContractSymbol $ FNFTContract.validatorHash ()) fracTN
          , GeneralParams.emittedFractions = 1000
          , GeneralParams.nftAC = emurgoToken
          , GeneralParams.remainedFractions = 1001
          }

  -- check wrong quantity of F-NFt
  let tx1 = lockTx uspIssuer ref nftVal fracVal1 validationVal fnftDatum
  mustFail . submitTx issuer $ tx1
  -- check wrong fractionAC
  let tx2 = lockTx uspIssuer ref nftVal fracVal validationVal fnftDatum2
  mustFail . submitTx issuer $ tx2
  -- check wrong remainedFractions
  let tx3 = lockTx uspIssuer ref nftVal fracVal validationVal fnftDatum3
  mustFail . submitTx issuer $ tx3
  -- correct tx
  let tx = lockTx uspIssuer ref nftVal fracVal validationVal fnftDatum
  submitTx issuer tx

  Plutus.Model.waitUntil 50
  noErrors

runChecks :: Property
runChecks = monadic property check
  where
    check = do
      isGood <- run $ testValues
      assert isGood

testValues :: Run Bool
testValues = do
  [issuer, user] <- setupUsers
  utxos <- utxoAt issuer
  let nftVal = fakeValue fake 1
      [(ref, out)] = utxos
      fracTN = TokenName $ calculateFractionTokenNameHash ref
      validationTN = TokenName $ fromString "FNFT_VALIDITY"
      fracVal = singleton (MintingContract.mintingContractSymbol $ FNFTContract.validatorHash ()) fracTN 1000
      validationVal =
        singleton (MintingContract.mintingContractSymbol $ FNFTContract.validatorHash ())  validationTN 1
  uspIssuer <- spend issuer nftVal
  let fnftDatum =
        GeneralParams.FNFTDatum
          { GeneralParams.fractionAC =
              assetClass (MintingContract.mintingContractSymbol $ FNFTContract.validatorHash ()) fracTN
          , GeneralParams.emittedFractions = 1000
          , GeneralParams.nftAC = emurgoToken
          , GeneralParams.remainedFractions = 1000
          }
  let tx = lockTx uspIssuer ref nftVal fracVal validationVal fnftDatum
  submitTx issuer tx
  Plutus.Model.waitUntil 50

  let fnftDatumNew =
        GeneralParams.FNFTDatum
          { GeneralParams.fractionAC =
              assetClass (MintingContract.mintingContractSymbol $ FNFTContract.validatorHash ()) fracTN
          , GeneralParams.emittedFractions = 1000
          , GeneralParams.nftAC = emurgoToken
          , GeneralParams.remainedFractions = 990
          }

  let fracValWithdraw =
        singleton (MintingContract.mintingContractSymbol $ FNFTContract.validatorHash ())  fracTN (10)
      fracRemainedValWithdraw =
        singleton (MintingContract.mintingContractSymbol $ FNFTContract.validatorHash ())  fracTN (990)

  withdrawUtxos <- utxoAt fnftContract
  let [(ref, out)] = withdrawUtxos

  let txWithdraw = withdrawTx user ref nftVal fracValWithdraw fracRemainedValWithdraw validationVal fnftDatum fnftDatumNew
  submitTx user txWithdraw
  Plutus.Model.waitUntil 50
  let fracValDeposit =
        singleton (MintingContract.mintingContractSymbol $ FNFTContract.validatorHash ())  fracTN (10)
      fracRemainedValDeposit =
        singleton (MintingContract.mintingContractSymbol $ FNFTContract.validatorHash ())  fracTN (1000)

  depositUtxos <- utxoAt fnftContract
  let [(ref, out)] = depositUtxos
  uspUser <- spend user fracValDeposit

  let txDeposit = depositTx uspUser ref 10 nftVal fracRemainedValDeposit validationVal  fnftDatumNew fnftDatum
  submitTx user txDeposit
  Plutus.Model.waitUntil 50

  let fracValBurn =
        singleton (MintingContract.mintingContractSymbol $ FNFTContract.validatorHash ())  fracTN (-1000)
      validationValBurn =
        singleton (MintingContract.mintingContractSymbol $ FNFTContract.validatorHash ()) validationTN (-1)
  burnUtxos <- utxoAt fnftContract
  let nft =
        find
          (\x -> do
             let (txOutRef', txOut') = x
                 value' = txOutValue txOut'
                 flatValues = flattenValue value'
             case find
                    (\(cs, tn, amt) ->
                       cs == MintingContract.mintingContractSymbol (FNFTContract.validatorHash ()) )
                    flatValues of
               Nothing -> False
               Just _  -> True)
          burnUtxos
  let (txOutRef, txOut) = fromJust nft
  let txBurn =
        unlockTx issuer txOutRef nftVal fracValBurn validationValBurn fnftDatum
  submitTx issuer txBurn
  Plutus.Model.waitUntil 50
  noErrors

runCheckWithdraw :: Property
runCheckWithdraw = monadic property check
  where
    check = do
      isGood <- run $ testValueWithdraw
      assert isGood

testValueWithdraw :: Run Bool
testValueWithdraw = do
  [issuer, user] <- setupUsers
  utxos <- utxoAt issuer
  let nftVal = fakeValue fake 1
      [(ref, out)] = utxos
      fracTN = TokenName $ calculateFractionTokenNameHash ref
      validationTN = TokenName $ fromString "FNFT_VALIDITY"
      fracVal = singleton (MintingContract.mintingContractSymbol $ FNFTContract.validatorHash ()) fracTN 1000
      validationVal =
        singleton (MintingContract.mintingContractSymbol $ FNFTContract.validatorHash ())  validationTN 1
  uspIssuer <- spend issuer nftVal
  let fnftDatum =
        GeneralParams.FNFTDatum
          { GeneralParams.fractionAC =
              assetClass (MintingContract.mintingContractSymbol $ FNFTContract.validatorHash ()) fracTN
          , GeneralParams.emittedFractions = 1000
          , GeneralParams.nftAC = emurgoToken
          , GeneralParams.remainedFractions = 1000
          }
  let tx = lockTx uspIssuer ref nftVal fracVal validationVal fnftDatum
  submitTx issuer tx
  Plutus.Model.waitUntil 50

  let fnftDatumNew =
        GeneralParams.FNFTDatum
          { GeneralParams.fractionAC =
              assetClass (MintingContract.mintingContractSymbol $ FNFTContract.validatorHash ()) fracTN
          , GeneralParams.emittedFractions = 1000
          , GeneralParams.nftAC = emurgoToken
          , GeneralParams.remainedFractions = 990
          }

  let fracValWithdraw =
        singleton (MintingContract.mintingContractSymbol $ FNFTContract.validatorHash ())  fracTN (10)
      fracValWithdraw1 =
        singleton (MintingContract.mintingContractSymbol $ FNFTContract.validatorHash ())  fracTN (11)
      fracRemainedValWithdraw =
        singleton (MintingContract.mintingContractSymbol $ FNFTContract.validatorHash ())  fracTN (990)
      fracRemainedValWithdraw1 =
        singleton (MintingContract.mintingContractSymbol $ FNFTContract.validatorHash ())  fracTN (989)

  withdrawUtxos <- utxoAt fnftContract
  let [(ref, out)] = withdrawUtxos

  -- check wrong withdraw quantity
  let txWithdraw1 = withdrawTx user ref nftVal fracValWithdraw1 fracRemainedValWithdraw1 validationVal fnftDatum fnftDatumNew
  mustFail . submitTx user $ txWithdraw1
  -- check wrong withdraw validation
  let txWithdraw2 = withdrawTx2 user ref nftVal fracValWithdraw fracRemainedValWithdraw validationVal fnftDatum fnftDatumNew
  mustFail . submitTx user $ txWithdraw2
   -- check wrong withdraw NFT
  let txWithdraw3 = withdrawTx3 user ref nftVal fracValWithdraw fracRemainedValWithdraw validationVal fnftDatum fnftDatumNew
  mustFail . submitTx user $ txWithdraw3
  -- correct tx
  let txWithdraw = withdrawTx user ref nftVal fracValWithdraw fracRemainedValWithdraw validationVal fnftDatum fnftDatumNew
  submitTx user txWithdraw

  Plutus.Model.waitUntil 50
  noErrors

runCheckDeposit :: Property
runCheckDeposit = monadic property check
  where
    check = do
      isGood <- run $ testValueDeposit
      assert isGood

testValueDeposit :: Run Bool
testValueDeposit = do
  [issuer, user] <- setupUsers
  utxos <- utxoAt issuer
  let nftVal = fakeValue fake 1
      [(ref, out)] = utxos
      fracTN = TokenName $ calculateFractionTokenNameHash ref
      validationTN = TokenName $ fromString "FNFT_VALIDITY"
      fracVal = singleton (MintingContract.mintingContractSymbol $ FNFTContract.validatorHash ()) fracTN 1000
      validationVal =
        singleton (MintingContract.mintingContractSymbol $ FNFTContract.validatorHash ())  validationTN 1
  uspIssuer <- spend issuer nftVal
  let fnftDatum =
        GeneralParams.FNFTDatum
          { GeneralParams.fractionAC =
              assetClass (MintingContract.mintingContractSymbol $ FNFTContract.validatorHash ()) fracTN
          , GeneralParams.emittedFractions = 1000
          , GeneralParams.nftAC = emurgoToken
          , GeneralParams.remainedFractions = 1000
          }
  let tx = lockTx uspIssuer ref nftVal fracVal validationVal fnftDatum
  submitTx issuer tx
  Plutus.Model.waitUntil 50

  let fnftDatumNew =
        GeneralParams.FNFTDatum
          { GeneralParams.fractionAC =
              assetClass (MintingContract.mintingContractSymbol $ FNFTContract.validatorHash ()) fracTN
          , GeneralParams.emittedFractions = 1000
          , GeneralParams.nftAC = emurgoToken
          , GeneralParams.remainedFractions = 990
          }

  let fracValWithdraw =
        singleton (MintingContract.mintingContractSymbol $ FNFTContract.validatorHash ())  fracTN (10)
      fracRemainedValWithdraw =
        singleton (MintingContract.mintingContractSymbol $ FNFTContract.validatorHash ())  fracTN (990)

  withdrawUtxos <- utxoAt fnftContract
  let [(ref, out)] = withdrawUtxos

  let txWithdraw = withdrawTx user ref nftVal fracValWithdraw fracRemainedValWithdraw validationVal fnftDatum fnftDatumNew
  submitTx user txWithdraw
  Plutus.Model.waitUntil 50
  let fracValDeposit =
        singleton (MintingContract.mintingContractSymbol $ FNFTContract.validatorHash ())  fracTN (10)
      fracRemainedValDeposit =
        singleton (MintingContract.mintingContractSymbol $ FNFTContract.validatorHash ())  fracTN (1000)

  depositUtxos <- utxoAt fnftContract
  let [(ref, out)] = depositUtxos
  uspUser <- spend user fracValDeposit

  let fnftDatumDeposit =
        GeneralParams.FNFTDatum
          { GeneralParams.fractionAC =
              assetClass (MintingContract.mintingContractSymbol $ FNFTContract.validatorHash ()) fracTN
          , GeneralParams.emittedFractions = 1000
          , GeneralParams.nftAC = emurgoToken
          , GeneralParams.remainedFractions = 999
          }

  -- check wrong deposit quantity
  let txDeposit1 = depositTx uspUser ref 10 nftVal fracRemainedValDeposit validationVal  fnftDatumNew fnftDatumDeposit
  mustFail . submitTx user $ txDeposit1
  -- correct tx
  let txDeposit = depositTx uspUser ref 10 nftVal fracRemainedValDeposit validationVal  fnftDatumNew fnftDatum
  submitTx user txDeposit
  Plutus.Model.waitUntil 50
  noErrors

runCheckBurn :: Property
runCheckBurn = monadic property check
  where
    check = do
      isGood <- run $ testValueBurn
      assert isGood

testValueBurn :: Run Bool
testValueBurn = do
  [issuer, user] <- setupUsers
  utxos <- utxoAt issuer
  let nftVal = fakeValue fake 1
      [(ref, out)] = utxos
      fracTN = TokenName $ calculateFractionTokenNameHash ref
      validationTN = TokenName $ fromString "FNFT_VALIDITY"
      fracVal = singleton (MintingContract.mintingContractSymbol $ FNFTContract.validatorHash ()) fracTN 1000
      validationVal =
        singleton (MintingContract.mintingContractSymbol $ FNFTContract.validatorHash ())  validationTN 1
  uspIssuer <- spend issuer nftVal
  let fnftDatum =
        GeneralParams.FNFTDatum
          { GeneralParams.fractionAC =
              assetClass (MintingContract.mintingContractSymbol $ FNFTContract.validatorHash ()) fracTN
          , GeneralParams.emittedFractions = 1000
          , GeneralParams.nftAC = emurgoToken
          , GeneralParams.remainedFractions = 1000
          }
  let tx = lockTx uspIssuer ref nftVal fracVal validationVal fnftDatum
  submitTx issuer tx
  Plutus.Model.waitUntil 50

  let fnftDatumNew =
        GeneralParams.FNFTDatum
          { GeneralParams.fractionAC =
              assetClass (MintingContract.mintingContractSymbol $ FNFTContract.validatorHash ()) fracTN
          , GeneralParams.emittedFractions = 1000
          , GeneralParams.nftAC = emurgoToken
          , GeneralParams.remainedFractions = 990
          }

  let fracValWithdraw =
        singleton (MintingContract.mintingContractSymbol $ FNFTContract.validatorHash ())  fracTN (10)
      fracRemainedValWithdraw =
        singleton (MintingContract.mintingContractSymbol $ FNFTContract.validatorHash ())  fracTN (990)

  withdrawUtxos <- utxoAt fnftContract
  let [(ref, out)] = withdrawUtxos

  let txWithdraw = withdrawTx user ref nftVal fracValWithdraw fracRemainedValWithdraw validationVal fnftDatum fnftDatumNew
  submitTx user txWithdraw
  Plutus.Model.waitUntil 50
  let fracValDeposit =
        singleton (MintingContract.mintingContractSymbol $ FNFTContract.validatorHash ())  fracTN (9)
      fracRemainedValDeposit =
        singleton (MintingContract.mintingContractSymbol $ FNFTContract.validatorHash ())  fracTN (999)

  depositUtxos <- utxoAt fnftContract
  let [(ref, out)] = depositUtxos
  uspUser <- spend user fracValDeposit

  let fnftDatumDepositRemain =
        GeneralParams.FNFTDatum
          { GeneralParams.fractionAC =
              assetClass (MintingContract.mintingContractSymbol $ FNFTContract.validatorHash ()) fracTN
          , GeneralParams.emittedFractions = 1000
          , GeneralParams.nftAC = emurgoToken
          , GeneralParams.remainedFractions = 999
          }

  let txDeposit = depositTx uspUser ref 9 nftVal fracRemainedValDeposit validationVal fnftDatumNew fnftDatumDepositRemain
  submitTx user txDeposit
  Plutus.Model.waitUntil 50

  let fracValBurn =
        singleton (MintingContract.mintingContractSymbol $ FNFTContract.validatorHash ())  fracTN (-1000)
      validationValBurn =
        singleton (MintingContract.mintingContractSymbol $ FNFTContract.validatorHash ()) validationTN (-1)
  burnUtxos <- utxoAt fnftContract
  let nft =
        find
          (\x -> do
             let (txOutRef', txOut') = x
                 value' = txOutValue txOut'
                 flatValues = flattenValue value'
             case find
                    (\(cs, tn, amt) ->
                       cs == MintingContract.mintingContractSymbol (FNFTContract.validatorHash ()) )
                    flatValues of
               Nothing -> False
               Just _  -> True)
          burnUtxos
  let (txOutRef, txOut) = fromJust nft
  -- check not enough F-NFT
  let txBurn =
        unlockTx issuer txOutRef nftVal fracValBurn validationValBurn fnftDatum
  mustFail . submitTx issuer $ txBurn

  let fracValDeposit1 =
        singleton (MintingContract.mintingContractSymbol $ FNFTContract.validatorHash ())  fracTN (1)
      fracRemainedValDeposit1 =
        singleton (MintingContract.mintingContractSymbol $ FNFTContract.validatorHash ())  fracTN (1000)

  depositUtxos1 <- utxoAt fnftContract
  let [(ref, out)] = depositUtxos1
  uspUser1 <- spend user fracValDeposit1
  let txDeposit1 = depositTx uspUser1 ref 1 nftVal fracRemainedValDeposit1 validationVal fnftDatumDepositRemain fnftDatum
  submitTx user txDeposit1
  Plutus.Model.waitUntil 50
  -- correct tx
  burnUtxos2 <- utxoAt fnftContract
  let nft2 =
        find
          (\x -> do
             let (txOutRef', txOut') = x
                 value' = txOutValue txOut'
                 flatValues = flattenValue value'
             case find
                    (\(cs, tn, amt) ->
                       cs == MintingContract.mintingContractSymbol (FNFTContract.validatorHash ()) )
                    flatValues of
               Nothing -> False
               Just _  -> True)
          burnUtxos2
  let (txOutRef, txOut) = fromJust nft2
  let txBurn2 =
        unlockTx issuer txOutRef nftVal fracValBurn validationValBurn fnftDatum
  submitTx issuer txBurn2
  Plutus.Model.waitUntil 50
  noErrors
