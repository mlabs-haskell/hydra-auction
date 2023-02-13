# Styleguide

* Imports separation **TBD**
* Single constructors are witten like this: `newtype MyType = MkMyType Integer`
* Acronims are written with normal case (like `Xml` or `Cli`)
* Merged comments on things to be changed later are marked with `FIXME`. `TODO` comments are reserved for local/draft changes and should not be merged.

# Documentation styleguide

* Use semantic newlines https://sembr.org/

# PR/commit guide

* Better separate refactoring PRs from actuall changes
* Tests should be successful at every commit: add expectFail or comment it in TestTree if something is broken temporarily.

# Reviewers checklist

* Check styleguide and PR guide
* Check for no unnecesary unsafe functions used and warnings/linters disabled
