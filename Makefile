.PHONY: format-check format lint-check lint clean

format-check: requires_nix_shell
	@sh format.sh check

format: requires_nix_shell
	@sh format.sh

lint-check: requires_nix_shell
	@sh lint.sh check

lint: requires_nix_shell
	@sh lint.sh

BUILD_PATH = "dist-newstyle"
clean:
	@[ ! -e $(BUILD_PATH) ] || rm -rf $(BUILD_PATH)

# # Target to use as dependency to fail if not inside nix-shell.
requires_nix_shell:
	@ [ "$(IN_NIX_SHELL)" ] || { \
	echo "The $(MAKECMDGOALS) target must be run from inside a nix shell"; \
	echo "    run 'nix develop .#onchain' first"; \
	false; \
	}
