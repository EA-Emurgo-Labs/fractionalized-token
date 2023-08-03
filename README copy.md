# token-marketplace
## Simple marketplace business
There are four cases will happen on market place:
+ BUY: user will make a request to buy the asset on our system, then the user
will interact with the Marketplace Contract to buy and update asset's owner.
+ WITHDRAW: user will make a request to withdraw the asset, then the user
will interact with the Marketplace Contract to withdraw.
+ CANCELL: the user pending trade on marketplace
+ SELL: the user change new price for asset
+ BUY_PARTIAL: the user can buy a part of the order
## Build contract
```
cabal build
cabal run marketplace-contract
``````