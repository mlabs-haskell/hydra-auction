#!/bin/bash
nix develop --command $(find /nix/store -maxdepth 1 -type d -iname '*haskell-language-server-exe*' | head -n 1| xargs -I % sh -c 'find  % -type f -name "haskell-language-server"') --lsp
