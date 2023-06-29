---
CIP: ?
Title: Add Blake 224 as new Plutus built-in
Category: Plutus
Status: Proposed
Authors:
    - Gregory Gerasev <uhbif19@mlabs.city>
Implementors: []
Discussions:
    - https://github.com/input-output-hk/plutus/issues/5369#issuecomment-1591198268
Created: YYYY-MM-DD
License: CC-BY-4.0

---

## Abstract

Add support for Blake 224 as builtin primitive.
This is required for converting PubKey to PubKeyHash onchain,
because Cardano does this with Blake 224.

## Motivation: why is this CIP necessary?

In Plutus transaction signatures only can be checked using PubKeyHash.
But datum signatures only can be checked using PubKey.

And there is no way to convert from PubKey to PubKeyHash onchain now.
That leaves no way to check if same actor signs transaction and any other data.

Such chekk is neccesary for Hydra applications with Delegate server architecture,
and any other applications in this signing datum and forming transaction
is done by different parties.

## Specification

Should work same way as existing `blake2b_256` but for Blake 224 hash.

## Rationale




## Backward Compatibility

At the Plutus Core level, implementing this proposal induces no backwards-incompatibility: the proposed new primitives do not break any existing functionality or affect any other builtins. Likewise, at levels above Plutus Core (such as PlutusTx), no existing functionality should be affected.

On-chain, this requires a hard fork.

## Path to Active

### Acceptance Criteria
<!-- Describes what are the acceptance criteria whereby a proposal becomes 'Active' -->

`blake2b_224` builtin exists.

### Implementation Plan
<!-- A plan to meet those criteria. Or `N/A` if not applicable. -->

Could be done just the same as `blake2b_256` builtin.

https://input-output-hk.github.io/plutus-apps/main/plutus-core/html/src/Data.ByteString.Hash.html#blake2b_256

It is already covered by `cardano-crypto-class`, so only API change is needed.

https://github.com/input-output-hk/cardano-base/blob/master/cardano-crypto-class/src/Cardano/Crypto/Hash/Blake2b.hs#L26


## Copyright
<!-- The CIP must be explicitly licensed under acceptable copyright terms. -->

[CC-BY-4.0]: https://creativecommons.org/licenses/by/4.0/legalcode
[Apache-2.0]: http://www.apache.org/licenses/LICENSE-2.0
