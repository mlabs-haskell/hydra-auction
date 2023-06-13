#!/bin/bash
cd $( dirname -- "${BASH_SOURCE[0]}")
nix develop -c bash -c "haskell-language-server-wrapper --lsp"
