{
  description = "hydra-auction";
  inputs = {
    cardano-hydra.url = "github:input-output-hk/hydra/9f1027e0fdff6765f5233f19c4639fdaa3558bfa";

    nixpkgs.follows = "cardano-hydra/nixpkgs";
    haskell-nix.follows = "cardano-hydra/haskellNix";
    iohk-nix.follows = "cardano-hydra/iohk-nix";
    flake-utils.follows = "cardano-hydra/flake-utils";
    cardano-node.follows = "cardano-hydra/cardano-node";

    flake-parts = {
      url = "github:hercules-ci/flake-parts";
      inputs.nixpkgs-lib.follows = "nixpkgs";
    };
    pre-commit-hooks-nix = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.nixpkgs-stable.follows = "nixpkgs";
    };
    hci-effects = {
      url = "github:hercules-ci/hercules-ci-effects";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-parts.follows = "flake-parts";
    };

  };

  outputs = inputs:
    let
      flakeModules = {
        haskell = ./nix/haskell;
        chap = ./nix/chap;
        utils = ./nix/utils;
      };
    in
    inputs.flake-parts.lib.mkFlake { inherit inputs; } ({ self, ... }: {
      imports = [
        inputs.pre-commit-hooks-nix.flakeModule
        inputs.hci-effects.flakeModule

        ./hydra-auction
      ] ++ (builtins.attrValues flakeModules);

      # `nix flake show --impure` hack
      systems =
        if builtins.hasAttr "currentSystem" builtins
        then [ builtins.currentSystem ]
        else inputs.nixpkgs.lib.systems.flakeExposed;

      herculesCI.ciSystems = [ "x86_64-linux" ];

      hercules-ci.flake-update = {
        enable = true;
        updateBranch = "hci/update-flake-lock";
        createPullRequest = true;
        autoMergeMethod = null;
        when = {
          minute = 45;
          hour = 12;
          dayOfWeek = "Sun";
        };
      };

      flake.flakeModules = flakeModules;

      perSystem =
        { config
        , pkgs
        , lib
        , self'
        , system
        , ...
        }: {
          _module.args.pkgs = import self.inputs.nixpkgs {
            inherit system;
            config.allowBroken = true;
          };

          pre-commit.settings = {
            hooks = {
              deadnix.enable = true;
              nixpkgs-fmt.enable = true;
              typos.enable = true;
              fourmolu.enable = true;
            };

            tools = {
              fourmolu = lib.mkForce (pkgs.callPackage ./nix/fourmolu {
                mkHaskellPackage = config.libHaskell.mkPackage;
              });
            };

            settings = {
              deadnix.edit = true;
            };

            excludes = [
              ".materialized"
            ];
          };

          devShells = {
            default = pkgs.mkShell {
              shellHook = config.pre-commit.installationScript;

              nativeBuildInputs = [
                pkgs.fd
              ];

              inputsFrom = [
                self'.devShells.hydra-auction
              ];
            };
          };
        };
    });
}
