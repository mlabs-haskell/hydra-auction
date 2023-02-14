# hydra-auction

## Overview

An implementation of a [delegated custodial auction](https://iohk.io/en/blog/posts/2023/01/20/implementing-auction-projects-using-hydra/) on top of Hydra.

The goal of this project is to provide a functioning implementation,
that can serve as a basis,
or reference for a commercial application seeking
to offer an infrastructure for building auctions on Cardano L2.

## How to build and run

This project can be built using nix.

```bash
nix build .
```

Will build the CLI application and link it to `./result/bin/hydra-auction`.

## Development

You can enter the development shell with:

```bash
nix develop .
```

You can run tests for the entire application by running:

```bash
nix build -L .#checks
```

## Specification

The full specification can be read [here](/doc/spec.md).

## Licensing

You are free to copy, modify, and distribute this software
under the terms of the Apache 2.0 license.

See the [LICENSE](/LICENSE) and [NOTICE](/NOTICE) files in this repository for details.
