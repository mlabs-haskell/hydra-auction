# Off-chain architecture and specification

In the Hydra Auction architecture,
commands/requests flow from people (Seller, Bidder, Delegate)
to systems (Frontend/CLI, Cardano node, Delegate server, Hydra node) as follows:

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

## Limitations of current approach

* All auctions users are from predefined list of actors
  (with fixed keys laying in `data/credentials`).
  This is only to simplify demonstration,
  no real limitation for that in scripts exists.
* All Hydra nodes know each others IPs before starting node
  (this is a current limitation of Hydra).
* Multiple Delegates per single Hydra node topology is not possible,
  due to Hydra API allowing any actions from any client.
* Single auction could be placed on single Hydra head.


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

For testing purposes local cardano-node could be used, which is started
using `docker-compose`.

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

Hydra node API should be only accessible by Delegate server.
API reference: https://hydra.family/head-protocol/api-reference

Delegate server may use this commands: `Init`, `Commit`, `NewTx`, `GetUTxO`, `Close`, `Fanout`.

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

The frontend CLI is a program that can be run by each of the seller and bidders.
It provides an interactive prompt
for them to submit their actions to interact with the auction.

<table><tr><td>

**start.** Start a frontend CLI session.

Command parameters:

- User for transactions
  (taken from predefined list of actors)

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

This action only makes sense when auction is running on devnet.

Request parameters:

- NFT recipient

</td></tr><tr></tr><tr><td>

**showUtxos.** Show the utxos owned by the current user.

**showAllUtxos.** Show the utxos of all users (Alice, Bob, etc.).

Request parameters: none.

</td></tr><tr></tr><tr><td>

**announceAuction.** Construct `AuctionTerms`
using the request parameters provided
(seller implicitly set to the request submitter),
and submit an L1 transaction to the Cardano node.

Request parameters:

- Auction ID
- Auction lot asset class
- Hydra Head ID
- Delegates

</td></tr><tr></tr><tr><td>

**startBiddingL2.** Execute a sequence of actions to allow bidding to start on L2.

Request parameters:

- Auction ID
- Delegate Server ID

This endpoint is equivalent to
calling startBiddingL1 and then calling moveStandingBidToL2.

</td></tr><tr></tr><tr><td>

**startBiddingL1.** If the request submitter is the seller
corresponding to the `AuctionTerms` of the Auction ID,
then submit an L1 transaction to the Cardano node
to start the bidding for the auction.

Request parameters:

- Auction ID

</td></tr><tr></tr><tr><td>

**moveStandingBidToL2.** Commit the standing bid to the Hydra Head
and coordinate the opening of the Hydra Head.

Request parameters:

- Auction ID
- Delegate server ID

This endpoint performs the following actions:

- Send a request to the chosen delegate server
to commit the standing bid utxo to the Hydra Head.

</td></tr><tr></tr><tr><td>

**newBidL1.** Submit a new bid as an L1 transaction to the Cardano node.

This is required for cases of non-honest delegate server,
which closed bidding to early.
Normaly all bids are placed in L2.

Request parameters:

- Auction ID
- Bid amount

</td></tr><tr></tr><tr><td>

**newBidL2.** Send a request to a given delegate server (chosen by the bidder)
to submit a new bid as an L2 transaction to the Hydra Head.

Request parameters:

- Bid amount
- Delegate server ID

Response:

- The bidder receives a Hydra Head L1 closing transaction
(with the latest L2 snapshot) signed by the delegate
to whom the newBidL2 request was forwarded,
which he can submit start from (biddingEnd - contestationPeriodDuration).

Cache this post-dated transaction for the bidder.

</td></tr><tr></tr><tr><td>

**closeHeadByBidder.** Submit the cached post-dated L1 closing transaction
to the Cardano node.

This is required on case of non-honest delegates,
normally they should do that automatically.

No request parameters required.

</td></tr><tr></tr><tr><td>

**fanoutByBidder.** Fan out the standing bid utxo from the Hydra Head,
so that it can be used in L1 transactions.

This is required on case of non-honest delegates,
normally they should do that automatically.

No request parameters required.

</td></tr></table>

### Delegate server

Things which should be done automatically:

* Hydra node is called to initialize Head, once Delegate is started.
* Once one Delegate (via Hydra node) made a commit with standing bid UTxO,
  all other Delegates place empty commits.
  Commiting status may be monitored on L2.
  All commits are required to open Hydra Head,
  which will be done by Hydra node automatically, once everyone commited.
  * If they do not - Head is aborted on `voucherExpriy`.
* Head is closed on `biddingEnd`.
  Otherwise bidders may place bids after it,
  because time validity cannot be enforced for transactions on L2.
* `Fanout` is triggered once contestation time ended.

<table><tr><td>

**commitStangingBid.** Used by `moveStandingBidToL2` in Frontend CLI.

Required params:

* Utxo which should be commited.
  * Currently commiting script addresses is not supported by Hydra.
    Details of implementation of this command depend on future API for them.
* Serialized `AuctionTerms`.
  Are required to know timing for actions performed automatically by
  * They should be checked by mathing `CurrencySymbol` to prevent
    timing manipulation by bidder.

</td></tr><tr></tr><tr><td>

**newBid.** Used by `newBidL2` in Frontend CLI.

</td></tr></table>