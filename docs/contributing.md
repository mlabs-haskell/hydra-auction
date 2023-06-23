# Styleguide

## Separate imports to section

* Prelude imports -- e.g. Haskell prelude, Plutus prelude, Hydra prelude, etc.
* Haskell imports -- any modules that a Haskell user outside of Cardano would import
  (e.g. Data.Map.Strict)
* Haskell test imports -- e.g. Tasty, Hedgehog, HUnit, etc.
* Cardano ledger imports -- modules from cardano-ledger
* Cardano node imports -- modules from cardano-node
* Plutus imports -- modules from plutus
* Hydra imports -- modules from hydra
* Hydra auction imports -- modules from `./src-lib`
* Hydra auction CLI imports -- modules from `./src-app/frontend-cli`
* Hydra auction test imports -- modules from `./src-test`

## Other guidelines

* Follow MLabs styleguide: https://github.com/mlabs-haskell/styleguide
* Single constructors are written like this:
  `newtype MyType = MkMyType Integer`
* Merged comments on things to be changed later are marked with `FIXME`.
  `TODO` comments are reserved for local/draft changes and should not be merged.
* Do not use `RecordWildcards`,
  apart for cases with very much fields used at same time.
  Reasoning: `RecordsWildCards` makes variable source implicit.
* Test selector should be writen in kebab case, like `bidder-buys`.
  Reasoning:
    * Easier to type shorter lowercased names.
    * Also, if test names don't have spaces,
    then you don't have to put them in "double-quotes" when selecting:
    `cabal test --test-options='-p bidder-buys'`

# Documentation styleguide

* Use semantic newlines https://sembr.org/

# PR/commit guide

* Better separate refactoring PRs from actual changes
  -- to ease reviewing and conflict resolution
* Tests should be successful at every commit:
  add expectFail or comment it in TestTree if something is broken temporarily.
  This may help with rebasing and bisection.

# Reviewers checklist

* Check styleguide and PR guide
* Check for no unnecesary unsafe functions used and warnings/linters disabled

# Debugging checklist

## Regular code

* Check on `Hydra.Prelude` imports. For example:
  * `threadDelay` uses seconds instread of microseconds
  * `try` uses `MonadCatch` from `io-classes`

## Plutus scripts

* Check out https://plutus.readthedocs.io/en/latest/troubleshooting.html
* Modules `Api` and `Context` should be imported from `PlutusLedgerApi.V2`,
  not `V1`.
