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
nix build .
```

Will build the CLI application and link it to `./result/bin/hydra-auction`.

## CLI usage

Different terms for auction are stored in JSON and auctions named with string.
Static part of params like stages timing and minmal bid/bid increment,
are stored in `examples/auction-congig`.
Dynamic part of params is calculated and stored on `auction-announced` command.
If you run it again for different lot dynamic part will be rewritten.

### Bidder wins case

1. Start repl for different actors in different terminals.
   You need at least one bidder (Alice in our example),
   and one bidder (Bob in our example).
   To start REPL run:
   `cabal run hydra-auction -- -a alice`
2. Run `prepare-for-demo -a alice` on Alice REPL
3. Run show-utxos to see which UTxO got Test NFT
4. Run `announce-auction` with this utxo, on Alice REPL, like
   `announce-auction -n foo -u f8ececf5a3589b316ecf8a2f72b1295d6319f36857708f1b0a904e03a5a709a6#0`
   From this time auction stages do begin.
5. Wait for `BiddingStartedStage`.
   When do `start-bidding -n foo` on Alice REPL.
6. Bidding started you can place bids, matching auction terms.
   They should be bigger than configStartingBid for first bid.
   They should be bigger than previous bid + configMinimumBidIncrement for
   all next bids.
   For example place first bid from Bob REPL:
   `new-bid -n foo -b 8000000`
7. (a) bidder-buys
   Run `bidder-buys -n foo` in Bob REPL.
   Bob gets his winning lot.
8. UTxOs can be cleaned up by seller in `VoucherExpiryStage`
   Run `cleanup -n foo`.

### Seller reclaims case


Same for all steps except 7.

7. (b) In case that winner does not take his lot, in `VoucherExpiryStage`,
   it can be reclaimed back by seller.
   Run `seller-reclaims -n foo` in Alice REPL.

## Development

You can enter the development shell with:

```bash
nix develop .
```

You can run tests for the entire application by running:

```bash
nix build -L .#checks
```

To run app with GHC warnings present you can use:

```bash
cabal run --ghc-option='-Wwarn'
```

## Specification

The full specification can be read [here](/doc/spec.md).

## Licensing

You are free to copy, modify, and distribute this software
under the terms of the Apache 2.0 license.

See the [LICENSE](/LICENSE) and [NOTICE](/NOTICE) files in this repository for details.
