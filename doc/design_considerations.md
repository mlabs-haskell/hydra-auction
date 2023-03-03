# Design considerations

## Ensuring that snapshots are closable

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

## Handling time on L2

The Cardano mainnet (L1) ledger keeps track of time
via slot numbers associated with each block in the chain,
and the duration between consecutive slot numbers is one second.
Each transaction can set a validity range of POSIX times
(converted internally to slots)
within which it can be added to a block,
and on-chain scripts can inspect this transaction validity range
to determine whether the transaction is valid
relative to time-dependent application logic.

The current Hydra Head (L2) implementation
keeps the slot number fixed at zero through all of its ledger state transitions.
The Hydra team plans to add support for time-constrained transactions,
but the specific design for this feature is still being developed.
The current proposal
([Hydra Issue #196](https://github.com/input-output-hk/hydra/issues/196))
is to periodically synchronize the slot number
(or corresponding POSIX time) from L1 to L2.
A more granular resolution for time is out-of-scope
for this feature (to be considered/added later).

In the Hydra-based auction, we really only need
to keep track of time within the Hydra Head
to determine when bidding can start and when it should end.
Bidding itself does not strictly require
tracking time explicitly within the Hydra Head:

- A new bid will replace the standing bid
that exists at the time that the new bid is validated
if the new bid exceeds the standing bid by the minimum bid increment.
- Simultaneous new bids will be resolved via utxo contention,
whereby the first bid among them to exceed the standing bid
and be multi-signed by the Hydra Head delegates
will replace the standing bid.


Simultaneous bids should occur less frequently on a Hydra Head because,
in principle, it should take much less time for the Head Head delegates
to achieve consensus on a ledger state transition
than it takes to achieve consensus on L1.
In other words, time resolution can effectively be much more granular
within a Hydra Head than on L1.

For bidding start/end times, the only option is
to introduce explicit time dependence into the auction protocol.
The bidding start time can be enforced in a straightforward way,
by locking the voucher NFT on L1 in a script
that only allows it to be committed to the Hydra Head
on or after the bidding start time.

Enforcing the bidding end time strictly would require
time to be explicitly tracked within L2
and for at least one Hydra Head delegate to reject bid transactions
that exceed this bidding deadline.
This isn’t feasible with the current Hydra Head protocol implementation,
which keeps the slot number fixed at zero.
However, we can work around this limitation
by combining the following three techniques:

1. Require all bid transactions to set a transaction validity interval
that starts at slot 0 and ends at the bidding end time.
This is enforced by the standing bid script
that determines whether a new bid can replace the standing bid.
2. Use the Hydra Head closing and contestation mechanisms
to force the auction towards L1 near the bidding end time.
3. Once the bidding end time elapses, allow the standing bid utxo
(which contains the voucher NFT)
to be spent by its bidder (i.e. the winning bidder)
to buy the auction lot from the seller.

The first technique does not affect the validity of transactions
within the Hydra Head (where the slot is always zero),
but it prevents further bids from being accepted
once the bidding end time elapses and the Hydra Head is fanned out.

In the second technique,
we use the Hydra Head closing and contestation mechanisms as follows
to force the auction toward L1 near the bidding end time:

- When a bidder submits a bid to a Hydra Head delegate,
via the delegate’s bid submission API,
the delegate will broadcast the bid transaction to the other delegates
and collect the multi-signature from them according to the Hydra Head protocol.
- When the multi-signature is obtained for the bid transaction,
the delegate responds to the bidder that the bid was accepted.
- In this response to the bidder,
the delegate attaches a transaction (signed by the delegate)
that would close the Hydra Head
with the ledger state achieved by the bid transaction.
- The validity range for this closing transaction
is set to start at the bidding end time, minus the contestation period duration.
If this closing transaction is submitted by the bidder to L1
as soon as its validity range begins,
then the contestation period for the Hydra Head closure
should conclude near the bidding end time.

Providing a signed closing transaction to the bidder at bid confirmation
results in a high probability
that the auction will move to L1 near the bidding end time
because the winning bidder has a very strong incentive
to end bidding in the Hydra Head as soon as he is allowed to do so.
Furthermore, the closing transaction does not need any further signatures
(from the bidder or anyone else),
which means that the bidder’s application frontend can submit it automatically
when it’s time.
The closing transaction can also be broadcast to the seller and other bidders,
in order to further improve the probability that the auction moves to L1
at the bidding end time.

The third technique ensures that the auction is resolved
as soon as the auction moves back to L1 and the bidding end time elapses,
without requiring any additional transactions from the Hydra Head delegates.
At that point, nothing blocks the winning bidder from buying the auction lot.

If the Hydra team adds support for timed transactions on L2,
then delegates would be able to enforce the bidding end time
directly in the Hydra Head ledger rules.
However, the delegates can already enforce this after the bidding end time
by rejecting bid submission API requests from bidders,
refusing to sign bid transactions from delegates,
and closing the Hydra Head.
Indeed, they should do these things regardless of whether
the bidding end time is validated by Hydra Head ledger rules.

The second technique still remains useful
even if timed transactions are supported on L2
because it allows bidders to close the Hydra Head near the bidding end time,
even if none of the delegates do it —
though we’re still relying on the delegates to contest the closing if needed
to ensure that the latest standing bid is fanned out.
Therefore, support for timed transactions on L2
would not particularly affect the auction design.

Note that the second technique does slightly degrade
the security properties of the Hydra Head protocol relative to the baseline.
If a bidder submits a closing transaction signed by a particular delegate,
then that delegate cannot submit a contestation transaction
and we must rely on the other delegates
to submit contestation transactions with the latest standing bid.
The original security properties would be recovered
if the Hydra Head participant that submitted a closing transaction
were also allowed to submit a contestation transaction.

## Open vs closed auctions

In this project, we implement a closed auction,
where the seller controls which bidders
can enter the auction before bidding starts.
This auction type is appropriate where the seller needs
to verify the list of bidders before allowing them to enter the auction,
and most auction platforms need to be able to support this type of auction.

The other type of auction is an open auction,
where bidders can freely enter the auction and place bids.
In order for open auctions to be feasible in our design,
we would have to replace the fixed security deposit mechanism
with proofs of full backing for bids.
Otherwise, any seller that creates an open auction
would have no way to vet bidders
to control the risk that he is willing to take on
that insincere bidders with insufficient deposits
sabotaging the auction.