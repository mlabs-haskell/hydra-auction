# Limitations and assumptions

## Limitations from current Hydra implementation state

* We use our custom Head commiting transaction constuction,
  instead of calling Hydra Node command because of:
  * Script commits are not supported by Hydra API.
    Commiting script UTxO is main part of our project.
* Querying different L2 protocol parameters is not supported.
  Now we just ignoring that and relying on delegate server
  knowing L2 protocol-parameters used by L2,
  because they are always the same and saved in repo.
  This should work well, because our topology presumes same operator
  for Delegate server and Hydra node.
  Issue on that: https://github.com/input-output-hk/hydra/issues/735
* Hydra Head can only host one auction and cannot be reused.
  * Later, when Hydra will support incremental commits and decommits,
    same Hydra Head and set of delegates can be reused
    for multiple Auctions.
    Issue on that: https://github.com/input-output-hk/hydra/issues/199
* All Hydra nodes should know each others IPs before starting a node.
  This limits cluster construction and makes them dependent
  on some form of communication or centralization.
  This issue covers the matter:
  https://github.com/input-output-hk/hydra/issues/240
* We depend on Hydra behaviour and responses, but they are not always clear.
  * Peer network timeouts are not always handled
    and overall strategy on them is not clear.
    https://github.com/input-output-hk/hydra/issues/188#issuecomment-1583110768
  * Hydra error responses strategy is not clear
  * Issues with persistence on node fails:
    * https://github.com/input-output-hk/hydra/issues/913
    * https://github.com/input-output-hk/hydra/issues/927

## Limitations inherent in Hydra

* If all delegates are malignant, they can create any bid state.
* Head progress depends on aviability of each Hydra Node.
  That means that any Node could break Head progress,
  and we cannot check if this was malignant action or not.
    * That means that our Delegate servers should be ready
      to close Head and clients should be ready to continue bidding on L1.
    * Head Id is fixed for auction, so we cannot move it to L2 again
      by using another Head.

## Limitations in Cardano ledger

* While we can (and do) disable script fees, collaterals are required by
  ledger any way.
  To work with that we commit collateral UTxOs by all delegates.

## Limitations inherent in our implementation

* Topology presumes delegate and hydra nodes are 1-1,
  and each pair is operated by same actor.

  Currently, multiple delegates cannot share a single Hydra node,
  due to the Hydra API allowing any actions from any client.
  So any client can close Head, and essegintally,
  Node should trust any client.

  But our model is dependent to 1-1 arch as well.
  Fees are distributed per delegate and node, and that means that
  Delegate actor should be the same as Node.

## Limitations in current implementation

* Protocol may stuck if there are too many delegates
  to fit them into fee distribution transation.
* We use very basic tx finality and timeouts strategy.
* Scripts are not optimized for Tx cost
  and reference scripts are not used.
* No model-based or property-based testing of protocol is performed.

## Limitations in UX/CLI made for simplicity of demo demonstration

* All auction users are from a predefined list of actors
  (with fixed keys laying in `data/credentials`).
  This is only to simplify demonstration,
  no real limitation for that in scripts exists.
* Hardcoded auction lot asset class is used.

# Tech debt

## Owned


* Testing
  * Our E2E tests do not cover most of non-succeseful usecases.
    This includes onchain logic coverage.
  * No full E2E test.
  * Slow L1 fixture due to `cardano-node` restarting every time.
  * We almost do not use PDD and unit-tests.
    Most of code is covered by E2E tests,
    most of `*-utils` are not covered separately.
* Type safety and exceptions
  * We sometimes use plain `Integers`
  for ADA amount (should be `Lovelace`) and time-units.
  * Networking exceptions are not handled.
* Architecture and libraries
  * We do not have generic tx constraints implementation.
    This would fix a lot of logic/code duplicity
    between `Tx.*` and `OnChain.*`.
    Having generic Tx parsing would simplify a lot of things as well.
  * Scripts/Tokens de-functionalization may work
    without manual `XAdress` newtypes, and overall be simpler.
  * Platform server utils may be defined more simple and generic.
    For example `Filter`s datatypes may be unifed with GADT.
* Naming and domain
  * Overall `UTxO` naming (single TxOut vs multiple) is very inconsistent.
  * `Tx.*` functionas are not very consistent in naming.
* Linting and infra
  * Issue: Add linting for 80 symbols in line infra #149
  * Not all MLabs styling guidelines are folowed (see issues)

## Depending on parent libs

* `Extras.Plutus` is still required.
  See: https://github.com/input-output-hk/plutus-apps/issues/1082#issuecomment-1586306926
  * And `aeson` orphans as well:
    https://github.com/input-output-hk/plutus/issues/5368
* `AuctionTerms` encoding
  * `PubKey` cannot be hashed on-chain:
    https://github.com/input-output-hk/plutus/issues/5369
  * `Head` is for off-chain use only:
    https://github.com/input-output-hk/hydra/issues/919
* `Ord Head` and something else is missing:
  * https://github.com/input-output-hk/hydra/pull/918 (merged in master)
  * https://github.com/input-output-hk/hydra/pull/929
* Hydra errors handling hacks in Delegate:
  https://github.com/input-output-hk/hydra/issues/839
