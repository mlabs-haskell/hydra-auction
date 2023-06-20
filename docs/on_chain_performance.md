# Per-tranaction execution stats

Taken manually from logs in L1 integration tests for devnet.

So maxTxExUnits from our genesis files should be:

* exUnitsMem: 14000000,
* exUnitsSteps: 10000000000

## General flow

### AnnounceAuction

Tx size % of max: 39.02  
CPU % of max: 3.54  
Memory % of max: 9.03  

### StartBidding

Tx size % of max: 51.43  
CPU % of max: 4.73  
Memory % of max: 12.10  

### NewBid

Tx size % of max: 45.66  
CPU % of max: 4.65  
Memory % of max: 10.58  

### BidderBuys

Tx size % of max: 51.09  
CPU % of max: 6.01  
Memory % of max: 15.41  

### DistributeFees

Tx size % of max: 26.92  
CPU % of max: 4.58  
Memory % of max: 11.82  

### Cleanup

Tx size % of max: 79.26  
CPU % of max: 5.97  
Memory % of max: 14.85  

## Deposits

BidderDeposit creation is scriptless.
It may be used in BidderBuys, so affects its execution costs.

### BidderBuys with deposit used

Tx size % of max: 94.96  
CPU % of max: 10.33  
Memory % of max: 25.70  

### SellerClaimsDeposit

Tx size % of max: 46.48  
CPU % of max: 4.14  
Memory % of max: 10.20  

## LosingBidder

Tx size % of max: 46.26  
CPU % of max: 3.46  
Memory % of max: 8.47  
