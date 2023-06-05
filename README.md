# hydra-auction

## Overview

This project is a collaboration between Input Output Global (IOG) and MLabs
to develop a reference implementation of an auction for Cardano
that uses the Hydra Head protocol.

The [Delegated Voucher Auction](https://iohk.io/en/blog/posts/2023/01/20/implementing-auction-projects-using-hydra/)
is an auction design that allows bidding to place within a Hydra Head.
It resolves the zero-sum game deadlock and logistical problems of
simpler designs where bidders directly participate in the Hydra Head protocol,
without requiring any third-parties to take custody
of seller or bidder funds.

We hope that this project will blaze the trail for the Cardano Community
to start using the Hydra suite of L2 protocols,
gaining the benefits of higher throughput, faster transaction finality,
and cheaper transaction fees.

## How to build and run

This project can be built using nix.

```bash
# while in a local checkout
nix build

# build it directly from git repo
nix build github:mlabs-haskell/hydra-auction/master

# run it immediately
nix run github:mlabs-haskell/hydra-auction/master -- -h
```

Will build the CLI application and link it to `./result/bin/hydra-auction`.

## Starting docker-compose environment

To setup the environment required for the demo, we have a compose file
that will start a single cardano-node,
3 hydra-nodes for the delgates,
and 3 instances of the delegate server.

Before starting the demo we will need to build the delegate server docker images.
This is done inside nix shell and can be done by `make build-docker`.
This command will build the image and load it as `hydra-auction-delegate:latest`.

We recommend call the `./spin-up-new-devnet.sh 1` script
to start all cluster components,
as it will take care of setting up the correct files for the cardano-node
and seeding the actors running the hydra-nodes with enough funds to manage the head lifecycle.

## CLI usage

Different terms for auction are stored in JSON and auctions are named with string.
The static parts of parameters: like the timing of stages and minimal bid, as well as
bid increment, are stored in `examples/auction-config`.
The dynamic part of the parameters is calculated and stored on `auction-announced`
command. If you run it again for a different lot, the dynamic part will be rewritten.

### Note on Actors

We have a set of known keys that we use for demo purposes.
The Cardano keys for these actors can be found under `data/credentials`.

We also have some hydra keys generated for the delegates, these are under `data/hydra-keys`.

In our demo, Oscar, Patricia and Rupert are the delegates running hydra nodes,
all other actors are meant to be either sellers or bidders.

### Prepare REPLs for demoing

Start a REPL for different actors in different terminals.

You need at least one seller (Alice in our example),
and one bidder (Bob in our example).
To start their REPLs run: `make demo-seller` and `make demo-bidder`.

Also run `make demo-monitor`.
This is ncurses CLI, which shows current state of auction and Head.

`make` shortcuts use params for local nodes started with `docker-compose`.
REPLs will be started inside docker image, to prevent issues on Mac (#180).

### Bidder wins case

1. Run `prepare-for-demo -a alice` on Alice's REPL
2. Run `announce-auction` , on Alice's REPL, like
   `announce-auction -n demo`

   We use an auction called `demo` that is stored in
   `./example/auction-config/demo.json` for example.
   You may create your own params in that dir and use that for auction.

   Now the auction time stages begin.
   You may see their status on `monitor` terminal.
3. Wait for `BiddingStartedStage`.
   After that, run `start-bidding -n demo` on Alice's REPL.

4. After bidding has started auction state still resides on L1.
   You can place bids on L1 before moving state on L2 if you want to.

   Normally, Head will close and return state on L2 on `BiddingEndedStage`.
   In that case you cannot continue to bet on L1.
   But in case L2 Head was halted before that, you can continue
   to place bets on L1.
   Halting happens then Head cannot reach consensus due to
   nodes/network failure or maliciant peer actions.

   Bid amount can be placed as long as it match the auction terms, which mean:
   - the first bid should be higher than `configStartingBid`
   - the next bid should always be higher than the previous bid + `configMinimumBidIncrement`

   For example place the first bid from Bob's REPL:
   `new-bid -n demo -b 100`, where the number in `100` is in ADA.
5. To move auction state on L2, run `move-to-l2 -n demo`.

   It will take some time for Head to do this,
   while placing bid on open Head is nearly instantaneous.
   You can track Head opening progress looking on `monitor` terminal.

   To place a bid on L2 run `new-bid-l2 -n demo -b 100`.
   Args/requirements are exactly the same as for L1 case.
6. (a) After the `BiddingEndedStage` has started,
   Head will be closed automatically.

   Bob can run `bidder-buys -n foo` in his REPL.
   Thus he will receive his lot and pay bid to seller.
7. After the `VoucherExpiryStage` has started, Alice can get back their `UTxO`s by running `cleanup -n foo`.

### Seller reclaims case

Same for all steps except 7.

6. (b) In case of the winner not taking their lot
   until the `VoucherExpiryStage`,
   it can be reclaimed back by its seller.
   For that run `seller-reclaims -n demo` in Alice's REPL.

### Reuse same auction config again

After those steps auction stage will forever stuck in `Cleanup`.
To reuse same config for new auction, just run `auction-announce` again.
This will overwrite dynamic config for old auction
and you can continue to rest of auction usecase.

## Development

You can enter the development shell with:

```bash
nix develop
```

You can run tests for the entire application by running:

```bash
nix build -Lv .#checks
```
or

`nix flake check -Lv --impure --allow-import-from-derivation`

To run the projects without errors on warnings, pass the `-Wwarn` flag as such:
```bash
cabal run --ghc-option='-Wwarn'
```

If you are having trouble using HLS with this project, you can
add the path to `script/run-hls.sh` into your LSP-extension config
as the path of the HLS binary.
This script will start HLS in our Nix environment.

You may also find other solutions here:
https://plutus.readthedocs.io/en/latest/troubleshooting.html#wrong-version

## Documentation

This project's documentation is organized (in the [doc](doc)) as follows:

- [domain_logic.md](doc/domain_logic.md) describes the terminology and models
for the Hydra Auction.
- [on_chain_spec.md](doc/on_chain_spec.md) describes the on-chain scripts.
- [off_chain_spec.md](doc/off_chain_spec.md) describes the architecture of off-chain components
and the APIs that they use to communicate with each other
and users.
- the [adr](doc/adr) folder contains [Architecture Decision Records](https://adr.github.io/) made so far in the project.

## Licensing

You are free to copy, modify, and distribute this software
under the terms of the Apache 2.0 license.

See the [LICENSE](/LICENSE) and [NOTICE](/NOTICE) files in this repository for details.
