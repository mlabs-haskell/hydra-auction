---
slug: 3
title: |
  3. Open vs closed auctions
authors: [George Flerovsky]
tags: [Accepted]
---

## Status

Accepted

## Context

In a closed auction,
the seller controls which bidders
can enter the auction before bidding starts.
This auction type is appropriate where the seller needs
to verify the list of bidders before allowing them to enter the auction,
and most auction platforms need to be able to support this type of auction.

In an open auction,
bidders can freely enter the auction and place bids.
In order for open auctions to be feasible in our design,
we would have to replace the fixed security deposit mechanism
with proofs of full backing for bids.
Otherwise, any seller that creates an open auction
would have no way to vet bidders
to control the risk that he is willing to take on
that insincere bidders with insufficient deposits
sabotaging the auction.

## Decision
In this project, we will first implement a closed auction.

Later on, if/when we implement a mechanism
for proving the full backing of bids,
we may implement an open auction.