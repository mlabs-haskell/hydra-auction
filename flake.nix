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
    bash-prompt = "\\[\\e[0m\\][\\[\\e[0;2m\\]nix-develop \\[\\e[0;1m\\]hydra-auction \\[\\e[0;93m\\]\\w\\[\\e[0m\\]]\\[\\e[0m\\]$ \\[\\e[0m\\]";
  };

  inputs = {
    # when you upgrade `hydra` input remember to also upgrade revs under
    # `source-repository-package`s in `cabal.project`
    hydra = {
      url = "ssh://git@github.com/input-output-hk/hydra?ref=ad3dd93ae3fcfee1be158a7042157d33d0ccd2ef";
      type = "git";
      submodules = true;
    };

    haskellNix.url = "github:input-output-hk/haskell.nix";
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
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
  };

  outputs =
    inputs@{ self
    , hydra
    , haskellNix
    , pre-commit-hooks
    , iohk-nix
    , CHaP
    , nixpkgs
    , flake-utils
    , cardano-node
    , ...
    }:
    let
      overlays = [
        haskellNix.overlay
        iohk-nix.overlays.crypto
        (final: prev: {
          hydraProject =
            final.haskell-nix.cabalProject {
              src = final.haskell-nix.haskellLib.cleanGit {
                src = ./.;
                name = "hydra-auction";
              };
              inputMap = {
                "https://input-output-hk.github.io/cardano-haskell-packages" = CHaP;
              };
              compiler-nix-name = "ghc8107";
              shell.tools = {
                cabal = "3.8.1.0";
                fourmolu = "0.4.0.0";
                haskell-language-server = "latest";
              };
              shell.buildInputs = with final; [
                nixpkgs-fmt
                haskellPackages.apply-refact
                haskellPackages.cabal-fmt
                haskellPackages.hlint
                cardano-node.packages.${prev.system}.cardano-node
                cardano-node.packages.${prev.system}.cardano-cli
                hydra.packages.${prev.system}.hydra-node
              ];
              modules = [
                # Set libsodium-vrf on cardano-crypto-{praos,class}. Otherwise
                # they depend on libsodium, which lacks the vrf functionality.
                ({ pkgs, lib, ... }:
                  # Override libsodium with local 'pkgs' to make sure it's using
                  # overriden 'pkgs', e.g. musl64 packages
                  {
                    packages.cardano-crypto-class.components.library.pkgconfig =
                      lib.mkForce [ [ pkgs.libsodium-vrf pkgs.secp256k1 ] ];

                    packages.cardano-crypto-praos.components.library.pkgconfig =
                      lib.mkForce [ [ pkgs.libsodium-vrf ] ];
                  }
                )
              ];
            };
        })
      ];
    in
    flake-utils.lib.eachDefaultSystem
      (system:
      let
        pkgs = import nixpkgs {
          inherit system overlays;
          inherit (haskellNix) config;
        };
        haskellNixFlake = pkgs.hydraProject.flake { };

        formatCheck = pkgs.runCommand "format-checks"
          {
            nativeBuildInputs = with pkgs; [
              fd
              nixpkgs-fmt
              haskellPackages.cabal-fmt
              haskellPackages.hlint
            ];
          }
          ''
            mkdir $out && cd $out
            cp ${self}/format.sh .
            sh format.sh check
            touch $out
          '';

        lintCheck = pkgs.runCommand "lint-checks"
          {
            nativeBuildInputs = with pkgs; [
              fd
              haskellPackages.hlint
            ];
          }
          ''
            mkdir $out && cd $out
            cp ${self}/lint.sh .
            cp ${self}/hlint.yaml .
            sh lint.sh check
            touch $out
          '';

        preCommitHook = pre-commit-hooks.lib.${system}.run
          {
            src = ./.;
            settings = { };
            tools = { };

            hooks = {
              nixpkgs-fmt.enable = true;
              fourmolu-format = {
                enable = true;
                name = "fourmolu format";
                entry = "make format-check";

                files = "";
                types = [ "file" ];
                excludes = [ ];
                language = "system";
                pass_filenames = false;
              };

              hlint-format = {
                enable = true;
                name = "hlint format";
                entry = "make lint-check";

                files = "";
                types = [ "file" ];
                excludes = [ ];
                language = "system";
                pass_filenames = false;
              };
            };
          };

        hydraChecks = builtins.mapAttrs
          (_: test: test.overrideAttrs (old: {
            nativeBuildInputs = old.nativeBuildInputs ++ [
              cardano-node.packages.${system}.cardano-node
              cardano-node.packages.${system}.cardano-cli
              hydra.packages.${system}.hydra-node
            ];
          }))
          haskellNixFlake.checks;
      in
      {
        packages = {
          default = haskellNixFlake.packages."hydra-auction:exe:hydra-auction";
          check = pkgs.runCommand "combined-test"
            {
              nativeBuildInputs = builtins.attrValues self.checks.${system};
            } "touch $out";
        };

        devShells = builtins.mapAttrs
          (_: test: test.overrideAttrs (old: {
            shellHook = old.shellHook + preCommitHook.shellHook;
          }))
          haskellNixFlake.devShells;

        checks = hydraChecks // {
          inherit formatCheck lintCheck;
        };

      }) // {
      herculesCI.ciSystems = [ "x86_64-linux" ];
    };
}
