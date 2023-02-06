#!/usr/bin/env sh

# Modified version of the lint.sh script available at
# https://github.com/mlabs-haskell/mlabs-tooling.nix/blob/main/lint.sh

set -xe
export LC_CTYPE=C.UTF-8
export LC_ALL=C.UTF-8
export LANG=C.UTF-8

refactor="yes"
mode=';'

if test "$#" -gt 0 && test "$1" = "check"
then
      refactor=""
      mode="+"
fi

# Using null breaks Plutus Tx
find . -type f -name '*.hs' ! -path '*/dist-newstyle/*' -exec \
	hlint \
      -XBangPatterns \
      -XTypeApplications \
      -XNondecreasingIndentation \
      -XPatternSynonyms \
      -XQualifiedDo \
      ${refactor:+"--refactor"} \
      ${refactor:+"--refactor-options=-i"} \
      -i "Use null" \
      --hint=hlint.yaml {} $mode
