# fractionalized-token
fractionalized Token
## Simple fractional business
- The main flow is that a user will lock their NFT token to FNFT contract and mint amount of FNFT token.
- With FNFT token, the user can withdraw them and trade on market.
- After someone collect all of FNFT token, he can deposit again on FNFT contract and burn all of FNFT token in order to get back the NFT.
### The project has two script:
- The minting script: 
    + This script will mint fractionalized NFT (F-NFT) token like normal token (in ETH, it is ERC20) and lock them to FNFT script by side a validation token. 
    + The validation helps us to identify utxo which should be consumed
    + This script will burn all F-NFT and validation token and make FNFT script unlock NFT to end user
- The FNFT script: 
    + This script handle F-NFT, validation token and NFT. 
    + This script lock NFT
    + The datum of tx output contain how much F-NFT release
### The actions on two script:
- Lock NFT and mint F-NFT: 
    + Minting script uses FNFT script hash like a built param to sure link right script address
    + The datum lock on FNFT script has to be show right quantity of F-NFT which release
    + F-NFT and validation token are the same symbol token
    + NFT lock on script FNFT and register on datum
- Unlock NFT
    + Minting script need all of F-NFT has the same quantity show on datum and validation token to burn
    + FNFT script check amount burn and release NFT
- Withdraw F-NFT
    + NFT and validation token are still locked in FNFT script
    + Change datum
- Deposit FN-FT
    + NFT and validation token are still locked in FNFT script
    + Change datum

## Build contract
```
cabal build
cabal run fnft-contract
``````
## Run property tesst
```
cabal run test
```