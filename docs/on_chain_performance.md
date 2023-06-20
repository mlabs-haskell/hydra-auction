# Per-tranaction execution stats

Taken manually from logs in L1 integration tests for devnet.

So maxTxExUnits from our genesis files should be:

* exUnitsMem: 14000000,
* exUnitsSteps: 10000000000

## General flow

### AnnounceAuction
Tx size % of max: 39.02587890625
CPU % of max: 3.54041683
Memory % of max: 9.035857142857143

### StartBidding
Tx size % of max: 51.434326171875
CPU % of max: 4.73428863
Memory % of max: 12.101042857142858

### NewBid
Tx size % of max: 45.660400390625
CPU % of max: 4.65233794
Memory % of max: 10.582571428571429

### BidderBuys
Tx size % of max: 51.092529296875
CPU % of max: 6.00875878
Memory % of max: 15.414114285714286

### DistributeFees

Tx size % of max: 26.922607421875
CPU % of max: 4.58534647
Memory % of max: 11.825871428571428

### Cleanup

Tx size % of max: 79.26025390625
CPU % of max: 5.97138646
Memory % of max: 14.854871428571428

## Deposits

BidderDeposit creation is scriptless.
It may be used in BidderBuys, so affects its execution costs.

### BidderBuys with deposit used

Tx size % of max: 94.964599609375
CPU % of max: 10.32686933
Memory % of max: 25.704085714285714

### SellerClaimsDeposit

Seller claiming bidder deposit
Tx size % of max: 46.484375
CPU % of max: 4.14413993
Memory % of max: 10.2039

## LosingBidder

Tx size % of max: 46.2646484375
CPU % of max: 3.45964679
Memory % of max: 8.472842857142858
