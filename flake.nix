{
  description = "Hydra Auction";

  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
    allow-import-from-derivation = true;
  };

  inputs = {
    # when you upgrade `hydra` input remember to also upgrade revs under `source-repository-package`s in `cabal.project`
    hydra = {
      url = "ssh://git@github.com/input-output-hk/hydra?ref=5ed00dfcd367d0390a774216035e1ea30dde5166";
      type = "git";
      submodules = true;
    };
    # We are using a forked version of haskellNix until this PR is merged: https://github.com/input-output-hk/haskell.nix/pull/1818
    haskellNix.url = "github:input-output-hk/haskell.nix/ff43e6ce6ad2a630da9d02faa8cf97794496bda6";
    # The "empty-flake" is needed until the following is fixed
    # https://github.com/input-output-hk/cardano-node/issues/4525 
    cardano-node = {
      url = "github:input-output-hk/cardano-node/a42ca8801ea31cb0b23a3f53dcc063ce4a5a0be5";
      inputs.cardano-node-workbench.follows = "empty";
      inputs.node-measured.follows = "empty";
    };
    empty.url = "github:mlabs-haskell/empty-flake";
    iohk-nix.follows = "hydra/iohk-nix";
    CHaP.follows = "hydra/CHaP";
    nixpkgs.follows = "hydra/nixpkgs";
    flake-utils.follows = "hydra/flake-utils";
  };

  outputs = inputs@{ self, hydra, haskellNix, iohk-nix, CHaP, nixpkgs, flake-utils, cardano-node, ... }:
    let
      overlays = [
        haskellNix.overlay
        iohk-nix.overlays.crypto
        (final: prev: {
          hydraProject =
            final.haskell-nix.cabalProject {
              src = final.haskell-nix.haskellLib.cleanGit { src = ./.; name = "hydra-auction"; };
              inputMap = {
                "https://input-output-hk.github.io/cardano-haskell-packages" = CHaP;
                "https://github.com/input-output-hk/hydra" = hydra;
              };
              compiler-nix-name = "ghc8107";
              shell.tools = {
                cabal = "3.8.1.0";
                fourmolu = "0.4.0.0";
                haskell-language-server = "latest";
              };
              shell.buildInputs = with final; [
                nixpkgs-fmt
                cardano-node.packages.${prev.system}.cardano-node
                cardano-node.packages.${prev.system}.cardano-cli
                hydra.packages.${prev.system}.hydra-node
              ];
              modules = [
                # Set libsodium-vrf on cardano-crypto-{praos,class}. Otherwise they depend
                # on libsodium, which lacks the vrf functionality.
                ({ pkgs, lib, ... }:
                  # Override libsodium with local 'pkgs' to make sure it's using
                  # overriden 'pkgs', e.g. musl64 packages
                  {
                  packages.cardano-crypto-class.components.library.pkgconfig = lib.mkForce [ [ pkgs.libsodium-vrf pkgs.secp256k1 ] ];
                  packages.cardano-crypto-praos.components.library.pkgconfig = lib.mkForce [ [ pkgs.libsodium-vrf ] ];
                  }
                )
                ];
            };
        })
      ];
    in flake-utils.lib.eachDefaultSystem (system : let
      pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
      haskellNixFlake = pkgs.hydraProject.flake { };
      in {
        packages.default = haskellNixFlake.packages."hydra-auction:exe:hydra-auction";
        inherit (haskellNixFlake) devShells;
        check = haskellNixFlake.checks."hydra-auction:test:hydra-auction-test".overrideAttrs (old: {
          nativeBuildInputs = [
            cardano-node.packages.${system}.cardano-node
            cardano-node.packages.${system}.cardano-cli
            hydra.packages.${system}.hydra-node
          ];
        });
      }) // {
      herculesCI = {
        ciSystems = [ "x86_64-linux" ];
        outputs = {
          packages = self.packages.x86_64-linux;
        };
      };
    };
}
