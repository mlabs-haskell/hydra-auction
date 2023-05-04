# Limitations and assumptions

## Limitations from current Hydra implementation state

* We use our custom Head commiting transaction constuction,
  instead of calling Hydra Node command because of two reasons:
  * Script commits are not supported by Hydra API.
    Commiting script UTxO is main part of our project.
  * Multiple UTxO commits not supported
    by both Hydra API and on-chain scripts.
    We need them to put commit collateral UTxO for delegate commiting standing bid.
* Hydra cannot validate tx time frame validity on-chain.
  That means that we cannot check bidding end time on L2.
  Instread we close Hean on that moment by all delegate servers.
  If they all are malignant, then they can create any bid state anyway.
  https://github.com/input-output-hk/hydra/issues/196
* Querying different L2 protocol parameters is not supported.
  Now we using one from L1, and that seems to work,
  but not sure that this is stable solution.
  Issue on that: https://github.com/input-output-hk/hydra/issues/735
* Hydra Head can only host one auction and cannot be reused.
  * Later, when Hydra will support incremental commits and decommits,
    same Hydra Head and set of delegates can be reused
    for multiple Auctions.
    Issue on that: https://github.com/input-output-hk/hydra/issues/199
* All Hydra nodes should know each others IPs before starting a node.
  This limits cluster construction and makes them dependent
  on some form of communication or centralization.
* Multiple delegates cannot share a single Hydra node,
  due to the Hydra API allowing any actions from any client.
  So we have one Hydra node to one Delegate server correspondence for now.

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

## Limitations in current implementation

* Protocol may stuck if there are too many delegates
  to fit them into fee distribution transation.
* Scripts are not optimized for Tx cost
  and reference scripts are not used.
* No model-based or property-based testing of protocol is performed.

## Limitations in UX/CLI made for simplicity of demo demonstration

* All auction users are from a predefined list of actors
  (with fixed keys laying in `data/credentials`).
  This is only to simplify demonstration,
  no real limitation for that in scripts exists.
* Hardcoded auction lot asset class is used.
