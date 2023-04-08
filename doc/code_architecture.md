No PAB is used here, we use Cardano-Api with custom helpers
for transaction construction. On-chain scripts are written
in pure Plutus.

Terminology:

* `Runner` - concrete monad with execution context over IO.

Utilities:

* `HydraAuction.Plutus.Extras` - extension of `PlutusExtra` package,
   which contains reimpelementation for important `plutus-ledger` stuff
* `HydraAuction.Plutus.Orphans` - orphans for Plutus stuff.
* `HydraAuctionUtils.Fixture` bunch of known actors (== Cardano keys) to use.
   Hydra does have one too, but it only have 3 general-use actors.

Library code:

* `HydraAuction.Types` domain types used by scripts
   as well as  our own `Natural` implementation,
   because we cannot add the dependency on `plutus-apps`
   that `plutus-numeric` would require.
* `HydraAuction.Addresses` - newtypes over our script addresses
* `HydraAuction.(Tx|Onchain).Common` contains both common domain code
   and utilities we use (to be fixed),
   for `Tx.*` or `OnChain.*` respectfully.
   All other submodules are grouped by on-chain script.
* `HydraAuction.Onchain.*` is for on-chain scripts implementation.
    * `HydraAuction.OnChain` itself contains their compilation,
       and defunctionalization - `scriptValidatorForTerms`.
* `HydraAuction.Tx.*` is for transaction construction
    * `HydraAuction.Runner` defines the Runner monad, which we use to interact with the Cardano-node

Exec code:

* `CLI.Actions` contains REPL actions
* `CLI.Config` contains work with dynamic and static config,
   which are JSON files parametrised by auction name.
    * Static config should be written before starting to work
     with auction.
    * Dynamic config is created by our REPL, when user does
      `announce-auction` REPL action.
    * Together, they determine `AuctionTerms` which fully define an auction
      define auction.
* `CLI.(Parsers|Prettyprinting)` do as their name says