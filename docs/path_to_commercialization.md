# Path to a full commercial solution

During the development of this project,
we found some areas where improving the API or capabilities of the hydra head
would allow DApps to offer more functionality.
Some of these issues have already been addressed by the hydra team, while other - more compelex - ones
are currently being designed/implemented, this document attepts to record the motivations behind those feature requests.

## Commit API

Currently the API that a hydra node offers to perform a commit, does not allow for UTxOs at an arbitrary script address to be included.
This means it is currently not easy to include all possible UTxOs inside a hydra head.
In our case, the auction is announced and set up on L1, this setup includes locking some UTxOs in a validator, and we then want to interact with these UTxOs inside the hydra head.
The good news is that this is not a limitation of the hydra protocol itself, but just in the external offered API. We were able to replicate the code to commit into the head in our own project, and extend to allow us to commit the required UTxOs.
There are valid technical reasons for the API to not offer this functionality yet, this item is currently being discussed with the hydra team which has already offered some possible solutions.
Improving on this would allow developers to write simpler code, and not have to worry about hydra internals if they require different kind of UTxOs in a hydra head.

Ref: https://github.com/input-output-hk/hydra/issues/215

## Query protocol parameters

The hydra node does not offer an API to query the protocol params that they are currently using.
This is unfortunate, as clients interacting with the node may require those parameters to construct the transactions they will then submit to the nodes.
Currently we work around this by hard coding the same pair of protocol parameters in both our nodes and clients, but it would be nicer to have the clients be able to query those directly from the node.

Ref: https://github.com/input-output-hk/hydra/issues/735

## Single auction per head

Incremental commits and decommits, are a feature that would allow funds entering - or exiting - the head, without needing to close and open a new one.
At the moment a Hydra Head can only host one auction, with the support for incremental commits and decommits it would be possible to re-use the same hydra head for several auctions.

Ref: https://github.com/input-output-hk/hydra/issues/199

## Configurable peers

All hydra nodes need to know each others hostnames/IPs at start time to populate the list of peers.

This can rapidly become hard to manage, with a large number of nodes, and requires an extra layer of coordination among hydra node operators to initialise the network.

In our case, this requies the delegates to be fully aware of each other, before even starting their hydra node. A solution which allows some form of discoverability between peers, would make the interactions between delegates easier.

Ref: https://github.com/input-output-hk/hydra/issues/240

## Collateral in hydra head

While we can (and do) disable script fees, collateral is still required for some transactions inside a hydra head.
This means that hydra head operators always need to commit some ADA inside the head to use as collateral.
In our use case, delegates have no reason to commit ADA into the head, but are still required to do so to cover collateral costs.
However, collateral would not need to exist inside a hydra head, as it is a mechansim of DDOS protection, and the participants in a hydra head have no incentive to DDOS the network (or to put it in another way: have easier ways to prevent the head from moving forward).

This limitation is possibly unfeasable to address, as the collateral requirement comes from the ledger rules, which hydra takes from Cardano itself.
