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
	nix build .#packages.x86_64-linux.delegateImage
	docker load < result

start-docker:
	./scripts/spin-up-new-devnet.sh

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

demo-monitor:
	./scripts/run-frontend-cli.sh -w demo  -d delegate-server-1:8001

demo-seller:
	./scripts/run-frontend-cli.sh -a alice --cardano-node-socket /node.socket --network-magic 42 -d delegate-server-1:8001

demo-bidder:
	./scripts/run-frontend-cli.sh -a bob --cardano-node-socket /node.socket --network-magic 42 -d delegate-server-2:8001

demo-bidder2:
	./scripts/run-frontend-cli.sh -a carol --cardano-node-socket /node.socket --network-magic 42 -d delegate-server-3:8001
