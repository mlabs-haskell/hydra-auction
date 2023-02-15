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
* Hydra auction imports -- modules from `./src`
* Hydra auction CLI imports -- modules from `./app`
* Hydra auction test imports -- modules from `./test`

## Other guidelines

* Single constructors are witten like this: `newtype MyType = MkMyType Integer`
* Acronims are written with normal case (like `Xml` or `Cli`)
* Merged comments on things to be changed later are marked with `FIXME`.
  `TODO` comments are reserved for local/draft changes and should not be merged.

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
