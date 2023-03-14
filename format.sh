#!/usr/bin/env sh

# Modified version of the format.sh script available at
# https://github.com/mlabs-haskell/mlabs-tooling.nix/blob/main/format.sh

set -xe
export LC_CTYPE=C.UTF-8
export LC_ALL=C.UTF-8
export LANG=C.UTF-8

pre-commit run fourmolu
pre-commit run cabal-fmt
