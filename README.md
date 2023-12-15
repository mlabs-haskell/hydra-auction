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

## Documentation

This project's documentation is organized 
(in the [doc](docs) folder) as follows:

- [domain_logic.md](docs/domain_logic.md) describes the terminology and models
for the Hydra Auction.
- [on_chain_spec.md](docs/on_chain_spec.md) describes the on-chain scripts.

## Licensing

You are free to copy, modify, and distribute this software
under the terms of the Apache 2.0 license.

See the [LICENSE](/LICENSE) and [NOTICE](/NOTICE) files in this repository for details.
