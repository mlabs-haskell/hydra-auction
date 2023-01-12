#!/usr/bin/env sh

# Modified version of the lint.sh script available at
# https://github.com/mlabs-haskell/mlabs-tooling.nix/blob/main/lint.sh

set -xe
export LC_CTYPE=C.UTF-8
export LC_ALL=C.UTF-8
export LANG=C.UTF-8

find . -type f -name '*.hs' ! -path '*/dist-newstyle/*' -exec \
	hlint \
      -XTypeApplications \
      -XNondecreasingIndentation \
      -XPatternSynonyms \
      -XQualifiedDo \
      --hint=hlint.yaml {} +
