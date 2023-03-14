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
which will start a single cardano-node and 3 hydra-nodes for the delgates.

We recommend using the `spin-up-new-devnet.sh` script
as it will take care of setting up the correct files for the cardano-node
and seeding the actors running the hydra-nodes with enough funds to manage the head lifecycle.

## CLI usage

Different terms for auction are stored in JSON and auctions named with string.
The static parts of parameters like the timing of stages and minimal bid as well as
bid increment, are stored in `examples/auction-config`.
The dynamic part of the parameters are calculated and stored on `auction-announced`
command. If you run it again for a different lot, the dynamic part will be rewritten.

### Note on Actors

We have a set of known keys that we use for demo purposes.
The Cardano keys for these actors can be found under `data/credentials`.

We also have some hydra keys generated for the delegates, these are under `data/hydra-keys`.

In our demo, Oscar, Patricia and Rupert are the delegates running hydra nodes,
all other actors are meant to be either sellers or bidders.

### Bidder wins case

1. Start a REPL for different actors in different terminals.
   You need at least one seller (Alice in our example),
   and one bidder (Bob in our example).
   To start REPL run:
   `cabal run hydra-auction -- -a alice --cardano-node-socket ./devnet/node.socket --network-magic 42`
2. Run `prepare-for-demo -a alice` on Alice's REPL
3. Run `show-utxos` to see which UTxO got Test NFT
4. Run `announce-auction` with this UTxO, on Alice's REPL, like
   `announce-auction -n some-auction` (for our demo, we use an auction called `demo` that is stored in
   `./example/auction-config/demo.json`)
   Now the auction time stages begin.
5. Wait for `BiddingStartedStage` by running `show-current-stage -n some-auction`
   After that, run `start-bidding -n some-auction` on Alice's REPL.
6. After bidding has started, you can place bids, as long as they match the auction terms, which are:
   - the first bid should be higher than `configStartingBid`
   - the next bid should always be higher than the previous bid + `configMinimumBidIncrement`
   For example place first bid from Bob's REPL:
   `new-bid -n foo -b 8000`, where the number in `b` is in ADA
7. (a) After the `BiddingEndedStage` has started, Bob can run`bidder-buys -n foo` in his REPL and receives
   his winning lot.
8. After the `VoucherExpiryStage` has started, Alice can get back their `UTxO`s by running `cleanup -n foo`.

### Seller reclaims case

Same for all steps except 7.

7. (b) In case of the winner not taking their lot, in the `VoucherExpiryStage`,
   it can be reclaimed back by its seller.
   Run `seller-reclaims -n foo` in Alice's REPL.

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

If you are having trouble using HLS with this project, you can use following
way. Add path to `script/run-hls.sh` into your LSP-extension config,
as path of HLS binary. This script will start HLS in our Nix environment.

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
