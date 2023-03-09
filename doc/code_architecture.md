No PAB is used here, we use Cardano-Api with custom helpers
for transaction construction. On-chain scripts are written
in pure Plutus.

Utilities:

* `HydraAuction.Plutus.Extras` - extension of `PlutusExtra` package,
   which contains reimpelementation for important `plutus-ledger` stuff
* `HydraAuction.Plutus.Orphans` - orphans for Plutus stuff.
* `HydraAuction.Fixture` bunch of known actors (== Cardano keys) to use.
   Hydra does have one too, but it only have 3 general-use actors.

Library code:

* `HydraAuction.Types` domain types used by scripts.
   haskell-language-server-wrapperAnd our `Natural` implementation, lol.
* `HydraAuction.Addresses` - newtypes over our script addresses
* `HydraAuction.(Tx|Onchain).Common` contains both common domain code
   and utilities we use (to be fixed),
   for `Tx.*` or `OnChain.*` respectfully.
   All other submodules are grouped by on-chain script.
* `HydraAuction.Onchain.*` is for on-chain scripts implementation.
    * `HydraAuction.OnChain` itself contains their compilation,
       and defunctionalization - `scriptValidatorForTerms`.
* `HydraAuction.Tx.*` is for transaction construction
    * `HydraAuction.Runner` is monad in which they do work.

Exec code:

* `CLI.Actions` contains REPL actions
* `CLI.Config` contains work with dynamic and static config,
   which are JSON files parametrised by auction name.
    * Static config should be written before starting to wor
     with auction.
    * Dynamic config is created by our REPL, when user does
      `announce-auction` REPL action.
    * They together do determine `AuctionTerms` which
      define auction.
* `CLI.(Parsers|Prettyprinting)` do as their name says