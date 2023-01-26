#!/usr/bin/env sh

# Modified version of the format.sh script available at
# https://github.com/mlabs-haskell/mlabs-tooling.nix/blob/main/format.sh

set -xe
export LC_CTYPE=C.UTF-8
export LC_ALL=C.UTF-8
export LANG=C.UTF-8

check=""
if test "$#" -gt 0 && test "$1" = "check"
then
	check="yes"
	fourmolu_mode="check"
	cabalfmt_mode="-c"
else
	fourmolu_mode="inplace"
	cabalfmt_mode="-i"
fi

find . -type f -name '*.hs' ! -path '*/dist-newstyle/*' -exec \
	fourmolu \
		-o-XTypeApplications \
		-o-XQualifiedDo \
		-o-XNondecreasingIndentation \
		-o-XPatternSynonyms \
		-o-XImportQualifiedPost \
		-o-XTemplateHaskell \
		-m "$fourmolu_mode" \
		--indentation 2 \
		--comma-style leading \
		--record-brace-space true  \
		--indent-wheres true \
		--respectful true  \
		--haddock-style multi-line  \
		--newlines-between-decls 1 \
		{} +

find . -type f -name '*.cabal' ! -path '*/dist-newstyle/*' -exec \
	cabal-fmt "$cabalfmt_mode" {} +
