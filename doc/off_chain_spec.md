# Off-chain architecture and specification

In the Hydra Auction architecture,
commands/requests flow from people (Seller, Bidder, Delegate)
to systems (Frontend/CLI, Cardano node, delegate server, Hydra node) as follows:

```mermaid
flowchart TB
  subgraph Auction App
    seller([Seller])
    bidder([Bidder])
    frontend(Frontend CLI)
  end

  subgraph Hydra-based Auction Hosting
    delegate([Delegate])
    delegateServer(Delegate server)
    hydraNode(Hydra node)
  end

  subgraph Cardano
    cardanoNode(Cardano node)
  end

  admin([Admin])

  admin -- start/stop -----> cardanoNode
  seller -- request --> frontend
  bidder -- request --> frontend
  frontend -- request --> cardanoNode
  frontend -- request --> delegateServer
  delegateServer -- request --> hydraNode
  delegate -- start/stop ---> delegateServer
  delegate -- start/stop ---> hydraNode
  hydraNode -- request --> cardanoNode
```

In the rest of this document,
we will describe each of these systems
and the request types that people can submit to those systems.

## Limitations of the current approach

* All auction users are from a predefined list of actors
  (with fixed keys laying in `data/credentials`).
  This is only to simplify demonstration,
  no real limitation for that in scripts exists.
* All Hydra nodes know each others' IPs before starting a node
  (this is a current limitation of Hydra).
* Multiple delegates cannot share a single Hydra node,
  due to the Hydra API allowing any actions from any client.
* An auction can only be hosted on a single Hydra Head
  and a Hydra Head can only host one auction.

## Off-chain workflow

A typical workflow for an auction should look as follows:

1. The delegates ensure that all daemon services are started
   (Delegate servers, Hydra nodes).
2. Delegates initialize the Hydra Head.
3. The seller announces the auction.
4. **(Bidding start time is reached)**
5. The seller starts the bidding phase.
6. A delegate moves the standing bid to L2.
7. The delegates open the Hydra Head.
8. Bidders may submit new bids on L2.
9. One of the delegates or bidders closes the Hydra Head on L1.
The delegates' Hydra nodes continuously monitor L1 for this closing transaction,
ready to submit contesting transactions if necessary.
10. The seller or one of the bidders or delegates
fans out the standing bid to L1.
11. **(Bidding end time is reached)**
12. The winning bidder may buy the auction lot.
13. **(Voucher expiry time is reached)**
14. If the auction lot has not been bought by the winning bidder,
then the seller reclaims the auction lot
and may claim the winning bidder’s bidder deposit.
15. One of the delegates distributes the auction fees to the delegates.
16. **(Cleanup time is reached)**
17. The seller spends the standing bid utxo and burns the voucher token.

## Services

### Cardano node

The Cardano node is responsible for
broadcasting transactions to the Cardano network.

In the real world, this would correspond to the node,
provided by a light-wallet backend.
Alternatively, for full decentralization, a user could in principle
choose to run their own Cardano node, without a trusted intermediary.

For testing purposes, a local Cardano node cluster (with one node)
can be started using `docker-compose`.

<table><tr><td>

**start.** Start a Cardano node, as a single-node development network (devnet)
or as a node on a known Cardano network (mainnet or testnet).

Command parameters:

- Cardano node config (state directory, tracer, etc.)
- Cardano network type = Devnet | Known network

</td></tr><tr></tr><tr><td>

**stop.** Stop the Cardano node.

</td></tr></table>

### Hydra node

The Hydra node is responsible for broadcasting L2 transactions to the Hydra Head
and participating in the Hydra Head consensus protocol
(including L1 Hydra Head transactions).

The Hydra node API should be only accessible to the delegate server.
It should not be directly accessible to sellers or bidders.

The delegate server may send the following commands to the Hydra node:
`Init`, `Commit`, `NewTx`, `GetUTxO`, `Close`, `Fanout`.

Hydra API reference: https://hydra.family/head-protocol/api-reference

### Delegate server

The delegate server is responsible for
responding to requests from the Frontend CLI,
constructing queries or transactions as necessary
to submit to the delegate’s Hydra node.

Each delegate should have its own delegate server and Hydra node running.

<table><tr><td>

**start.** Start a delegate server and an associated Hydra Node.

Command parameters:

- Delegate server ID
- Delegate server config

</td></tr><tr></tr><tr><td>

**stop.** Stop the delegate server and its associated Hydra node.

</td></tr></table>

### Frontend CLI

The frontend CLI is a program that can be run by sellers and bidders.
It provides an interactive prompt
for them to submit their actions to interact with the auction.

<table><tr><td>

**start.** Start a frontend CLI session.

Command parameters:

- User
  (selected from a predefined list of actors)

</td></tr><tr></tr><tr><td>

**stop.** Quit the frontend CLI session.

</td></tr></table>

## API

### Frontend CLI

The `AuctionTerms` for the Auction ID are cached in the auction state directory.

<table><tr><td>

**prepare.** Distribute ADA from the faucet
to all the potential users in the auction (Alice, Bob, etc.)
and provide a freshly minted NFT to the recipient,
so that the NFT can be put up for auction.

This action is intended for testing purposes on local devnets.
It has no effect on mainnet or public testnets.

Request parameters:

- NFT recipient

</td></tr><tr></tr><tr><td>

**showUtxos.** Show the utxos owned by the current user.

**showAllUtxos.** Show the utxos of all users (Alice, Bob, etc.).

Request parameters: none.

</td></tr><tr></tr><tr><td>

**announceAuction.** Used by seller.

Construct `AuctionTerms` using the request parameters provided
(seller implicitly set to the request submitter),
and submit an L1 transaction to the Cardano node.

From this moment timing of stages begins.

Request parameters:

- Auction ID
- Auction lot asset class
- Hydra Head ID
- Delegates

</td></tr><tr></tr><tr><td>

**createBidDeposit.** Used by bidders, to place deposit.

Performed before BiddingStart.

Approved bidders will be selected from bidders,
which placed deposit big enough.

Request parameters:

- Auction ID
- Deposit amount

</td></tr><tr></tr><tr><td>

**getBiddersWithSufficientDeposits.** Used by the seller.

This query shows which bidders have made sufficient deposits for the auction. The seller can consult this list to decide on which bidders to include in the approved bidders list, when starting the auction.

Request parameters:

- Auction ID

Response:

- List of bidders

</td></tr><tr></tr><tr><td>

**startBiddingL2.** Used by seller.

Performed after BiddingStart and before BiddingEnd.

Execute a sequence of actions to allow bidding to start on L2.

Request parameters:

- Auction ID
- Delegate Server ID
- Approved Bidders

This endpoint is equivalent to
calling startBiddingL1 and then calling moveStandingBidToL2.

</td></tr><tr></tr><tr><td>

**startBiddingL1.** Used by seller.

Performed after BiddingStart and before BiddingEnd.

If the request submitter is the seller
corresponding to the `AuctionTerms` of the Auction ID,
then submit an L1 transaction to the Cardano node
to start the bidding for the auction.

Request parameters:

- Auction ID
- Approved Bidders

</td></tr><tr></tr><tr><td>

**moveStandingBidToL2.** Used by seller.

Commit the standing bid to the Hydra Head
and coordinate the opening of the Hydra Head.

Request parameters:

- Auction ID
- Delegate server ID

This endpoint performs the following actions:

- Send a request to the chosen delegate server
to commit the standing bid utxo to the Hydra Head.

</td></tr><tr></tr><tr><td>

**newBidL1.** Submit a new bid as an L1 transaction to the Cardano node.

Performed after BiddingStart and before BiddingEnd.

This request is needed for the fallback scenario when
the standing bid is not on L2 because either
it was never moved there
or the Hydra Head closed prematurely.

Normally, all bids should be submitted to L2.

Request parameters:

- Auction ID
- Bid amount

</td></tr><tr></tr><tr><td>

**newBidL2.** Send a request to a given delegate server (chosen by the bidder)
to submit a new bid as an L2 transaction to the Hydra Head.

Performed after BiddingStart and before BiddingEnd.
Timing could not be enforced on L2, but delegate will close Head
on BiddingEnd.

Request parameters:

- Auction ID
- Bid amount
- Delegate server ID

Response:

- The bidder receives a Hydra Head L1 closing transaction
(with the latest L2 snapshot) signed by the delegate
to whom the newBidL2 request was forwarded,
which he can submit start from (biddingEnd - contestationPeriodDuration).

Cache this post-dated transaction for the bidder.

</td></tr><tr></tr><tr><td>

**bidderBuys.** Used by the winning bidder.

Peformed after BiddingEnd and before VoucherExpiry.

Submits a transaction that spends the auction escrow utxo
with the `BidderBuys` redeemer
and spends (if available) the bidder's deposit
with the WinningBidder redeemer.

Request parameters:

- Auction ID

</td></tr><tr></tr><tr><td>

**sellerReclaims.** Used by the seller.

Performed after VoucherExpiry.

Submits a transaction that spends the auction escrow utxo
with the `SellerReclaims` redeemer and spends
(it is guaranted to be available)
the winning bidder's deposit
with the SellerClaimsDeposit redeemer.

Request parameters:

- Auction ID

</td></tr><tr></tr><tr><td>

**refundDeposit.** Used by losing bidders.

Peformed after BiddingEnd.

Submits a transaction that spends a bidder deposit
with the `LosingBidder` redeemer.

Request parameters:

- Auction ID

</td></tr><tr></tr><tr><td>


**closeHeadByBidder.** Submit the cached post-dated L1 closing transaction
to the Cardano node.

This request is needed for the fallback scenario
when the delegates fail to close the Hydra Head
in time for the contestation period to end
by the bidding end time.
In that scenario, a bidder can use this request
to force the Hydra Head to close, preventing
the Hydra Head from staying open
significantly past the bidding end time.

No request parameters are required.

</td></tr><tr></tr><tr><td>

**fanoutByBidder.** Fan out the standing bid utxo from the Hydra Head,
so that it can be used in L1 transactions.

This request is needed in case the delegates'
hydra nodes fail to fan out the standing bid
after the Hydra Head closes.
Normally, that should happen automatically.

No request parameters are required.

</td></tr></table>

### Delegate server

Things which should be done automatically:

* When a delegate server starts,
  its corresponding Hydra node should start
  and request the initialization of the Hydra Head.
* When one of the delegates makes a commit
  (via Hydra node) with the standing bid UTxO,
  all of the other delegates should make empty commits.
  They should be able to do this by monitoring
  the commit messages on L2.
* The Hydra Head should be opened automatically
  as soon as all Hydra nodes commit.
* If the Hydra Head isn't opened before the bidding end time,
  it should be aborted.
* By the bidding end time, the Hydra Head should be closed
  and all contestation requests should be exhausted.
  Otherwise, bidders would be able to submit new bids on L2
  past the bidding end time.
* `Fanout` should be triggered when the contestation
  period ends.

<table><tr><td>

**commitStandingBid.** Used by `moveStandingBidToL2` in Frontend CLI.

Request parameters:

* The Utxo that should be commited.
* Serialized `AuctionTerms`.

The `AuctionTerms` should be validated by the delegate
by computing a `CurrencySymbol` of the voucher minting policy
applied to these terms
and comparing it to the voucher token
contained in the utxo to be committed.

**Note:** Implementation for this API request is currently blocked
because Hydra nodes do not support creating transactions
to commit utxos from script addresses to a Hydra Head.
The implementation details of this request depend on
how this feature will be implemented in Hydra.

</td></tr><tr></tr><tr><td>

**newBid.** Construct and submit a `NewBid` transaction on L2,
based on the information provided in the request parameters.

Request parameters:
- Auction ID
- Bid amount

This request is sent by the Frontend CLI when it receives
a `newBidL2` request from a bidder.

</td></tr></table>