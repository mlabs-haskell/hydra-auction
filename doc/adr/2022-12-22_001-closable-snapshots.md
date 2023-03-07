---
slug: 1
title: |
  1. Closable snapshots
authors: [George Flerovsky]
tags: [Accepted]
---

## Status

Accepted

## Context

The current Hydra Head implementation does not support
minting and burning tokens in the fanout transaction,
and there is a limit to the number of utxos
that can be produced by the fanout transaction.
This means that it is impossible to close a Hydra Head
if its ledger contains newly minted or burned tokens,
or if it contains more utxos than can be supported by the fanout transaction.
The Hydra team is planning to address these limitations
by excluding “phantom tokens” (minted/burned on L2) from Hydra Head snapshots
([Hydra Issue #358](https://github.com/input-output-hk/hydra/issues/358))
and by only signing snapshots that are known to be closable
([Hydra Issue #370](https://github.com/input-output-hk/hydra/issues/370)).

These limitations do not affect the Hydra-based auction
because we do not mint or burn any tokens within the Hydra Head
and we only commit and fan out one utxo to/from the Hydra Head.

## Decision

Do nothing.