## Overview

This project is a collaboration between Input Output Global (IOG) and MLabs
to develop a reference implementation of an auction for Cardano
that uses the Hydra Head protocol.

The [Delegated Voucher Auction](https://iohk.io/en/blog/posts/2023/01/20/implementing-auction-projects-using-hydra/)
is an auction design that allows bidding to place within a Hydra Head.
It resolves the zero-sum game deadlock and logistical problems of
simpler designs where bidders directly participate in the Hydra Head protocol,
without requiring any third parties to take custody
of seller or bidder funds.

We hope that this project will blaze the trail for the Cardano Community
to start using the Hydra suite of L2 protocols,
gaining the benefits of higher throughput, faster transaction finality,
and cheaper transaction fees.


## Documentation

This project's documentation is organized as follows:

- [This document](https://github.com/mlabs-haskell/hydra-auction/blob/staging/docs/domain_logic.md) describes the terminology and models for the Hydra Auction.
- The [on-chain spec](https://github.com/mlabs-haskell/hydra-auction/blob/staging/docs/on_chain_spec.md) describes the on-chain scripts
- The [off-chain spec](https://github.com/mlabs-haskell/hydra-auction/blob/staging/docs/off_chain_spec.md) describes the architecture of off-chain components and the APIs that they use to communicate with each other and users.
- [This file](./code_architecture.md) describes how the project is structured.
- List of [Architecture Decision Records](https://adr.github.io/) made so far in the project.
{% for adr in site.adr %}
  - [{{ adr.title }}]({{ adr.url }})
{% endfor %}
- [Here](./path_to_commercialization.md) we describe some possible extensions that may be implemented in the future as the capabilities of hydra evolve

