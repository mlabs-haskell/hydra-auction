SHELL := /bin/bash

.PHONY:
	check
	hoogle
	build
	test
	repl
	format-check
	format
	lint-check
	lint
	clean
	requires_nix_shell

check:
	@nix flake check -Lv --impure --allow-import-from-derivation

hoogle: requires_nix_shell
	@hoogle server --local --port=8070 > /dev/null &

build: requires_nix_shell
	@cabal v2-build

test: requires_nix_shell
	@cabal v2-test --test-show-details=direct

repl: requires_nix_shell
	@nix develop -c cabal new-repl

format-check: requires_nix_shell
	@sh format.sh check

format: requires_nix_shell
	@sh format.sh

lint-check: requires_nix_shell
	@sh lint.sh check

lint: requires_nix_shell
	@sh lint.sh

build-docker:
	nix build .#packages.x86_64-linux.cliImage
	docker load < result
	mv result build-images/hydra-auction-cli
	nix build .#packages.x86_64-linux.delegateImage
	docker load < result
	mv result build-images/hydra-auction-delegate

start-cluster:
	./scripts/spin-up-new-devnet.sh 1

BUILD_PATH = "dist-newstyle"
NODE_STATE_PATH = "node-state"
clean:
	@[ ! -e $(BUILD_PATH) ] || rm -rf $(BUILD_PATH)
	@[ ! -e $(NODE_STATE_PATH) ] || rm -rf $(NODE_STATE_PATH)

# # Target to use as dependency to fail if not inside nix-shell.
requires_nix_shell:
	@ [ "$(IN_NIX_SHELL)" ] || { \
	echo "The $(MAKECMDGOALS) target must be run from inside a nix shell"; \
	echo "    run 'nix develop .' first"; \
	false; \
	}

faucet-address:
	bash -c 'source scripts/setup-envs.sh; frontend-cli-faucet-command "show-address"'

demo-monitor:
	bash -c 'source scripts/setup-envs.sh; frontend-cli "1" "-w" "demo"'

demo-seller:
	bash -c 'source scripts/setup-envs.sh; frontend-cli-repl "1" "-a" "alice"'

demo-bidder:
	bash -c 'source scripts/setup-envs.sh; frontend-cli-repl "2" "-a" "bob"'

demo-bidder2:
	bash -c 'source scripts/setup-envs.sh; frontend-cli-repl "2" "-a" "bob"'
