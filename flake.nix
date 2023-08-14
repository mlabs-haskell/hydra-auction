{
  description = "Hydra Auction";
  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
      "https://hydra-node.cachix.org"
      "https://cardano-scaling.cachix.org"
      "https://cache.zw3rk.com"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "hydra-node.cachix.org-1:vK4mOEQDQKl9FTbq76NjOuNaRD4pZLxi1yri31HHmIw="
      "cardano-scaling.cachix.org-1:RKvHKhGs/b6CBDqzKbDk0Rv6sod2kPSXLwPzcUQg9lY="
      "loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk="
    ];

    allow-import-from-derivation = true;
    bash-prompt = "\\[\\e[0m\\][\\[\\e[0;2m\\]nix-develop \\[\\e[0;1m\\]hydra-auction \\[\\e[0;93m\\]\\w\\[\\e[0m\\]]\\[\\e[0m\\]$ \\[\\e[0m\\]";
  };

  inputs = {
    # when you upgrade `hydra` input remember to also upgrade revs under
    # `source-repository-package`s in `cabal.project`
    hydra.url = "github:input-output-hk/hydra/0a2cdbd243a061aa52f28e992ff571ea80e2e1aa";
    haskellNix.follows = "hydra/haskellNix";
    # The "empty-flake" is needed until the following is fixed
    # https://github.com/input-output-hk/cardano-node/issues/4525
    cardano-node.follows = "hydra/cardano-node";
    empty.url = "github:mlabs-haskell/empty-flake";
    iohk-nix.follows = "hydra/iohk-nix";
    CHaP.follows = "hydra/CHaP";
    nixpkgs.follows = "hydra/nixpkgs";
    flake-utils.follows = "hydra/flake-utils";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
  };

  outputs =
    { self
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
      # nix flake (show|check) --allow-import-from-derivation --impure

      systems =
        if builtins.hasAttr "currentSystem" builtins
        then [ builtins.currentSystem ]
        else nixpkgs.lib.systems.flakeExposed;
      overlays = [
        haskellNix.overlay
        # This overlay contains libsodium and libblst libraries
        iohk-nix.overlays.crypto
        # This overlay contains pkg-config mappings via haskell.nix to use the
        # crypto libraries above
        iohk-nix.overlays.haskell-nix-crypto
        (final: prev:
          let
            # Hack from Hydra codebase
            # haskell-language-server = final.haskell-nix.tool "ghc8107" "haskell-language-server" rec {
            #   src = final.haskell-nix.sources."hls-1.10";
            #   cabalProject = builtins.readFile (src + "/cabal.project");
            #   sha256map."https://github.com/pepeiborra/ekg-json"."7a0af7a8fd38045fd15fb13445bdcc7085325460" = "sha256-fVwKxGgM0S4Kv/4egVAAiAjV7QB5PBqMVMCfsv7otIQ=";
            # };
          in
          {
            hydraProject =
              final.haskell-nix.cabalProject {
                src = final.haskell-nix.haskellLib.cleanGit {
                  src = ./.;
                  name = "hydra-auction";
                };
                inputMap."https://input-output-hk.github.io/cardano-haskell-packages" = CHaP;

                compiler-nix-name = "ghc928";

                shell.tools = {
                  cabal = "3.10.1.0";
                  # fourmolu = "0.13.0.0";
                };
                shell.buildInputs = with final; [
                  haskell-language-server
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
                  ({ ... }:
                    # Override libsodium with local 'pkgs' to make sure it's using
                    # overriden 'pkgs', e.g. musl64 packages
                    {
                      # disable the dev flag in the nix code, s.t. warnings are becoming errors
                      # the dev flag implies PlutusTx defer plugin error and disabling -Werror
                      packages.hydra-auction.allComponent.configureFlags = [ "-f-dev" ];
                    }
                  )
                ];
              };
          })
      ];
      removeIncompatibleAttrs = pkgNames: attrName: systems: flake:
        let
          f = attrset: system:
            nixpkgs.lib.updateManyAttrsByPath [{
              path = [ "${attrName}" "${system}" ];
              update = set: (builtins.removeAttrs set pkgNames);
            }]
              attrset;
        in
        builtins.foldl' f flake systems;
    in
    removeIncompatibleAttrs [ "cliImage" "delegateImage" ] "packages" (builtins.filter (sys: sys != "x86_64-linux") systems)
      (flake-utils.lib.eachSystem systems
        (system:
        let
          pkgs = import nixpkgs {
            inherit system overlays;
            inherit (haskellNix) config;
          };
          haskellNixFlake = pkgs.hydraProject.flake { };

          preCommitHook = pre-commit-hooks.lib.${system}.run
            {
              src = ./.;
              hooks = {
                nixpkgs-fmt.enable = true;
                statix.enable = true;
                deadnix.enable = true;
                fourmolu.enable = true;
                hlint.enable = true;
                cabal-fmt.enable = true;
              };
              tools.fourmolu = pkgs.lib.mkForce pkgs.haskell.packages.ghc92.fourmolu;
              settings = {
                ormolu.defaultExtensions = [
                  "BangPatterns"
                  "TypeApplications"
                  "QualifiedDo"
                  "NondecreasingIndentation"
                  "PatternSynonyms"
                  "ImportQualifiedPost"
                  "TemplateHaskell"
                ];
              };
            };

          wrapTest = test: pkgs.runCommand "${test.name}-wrapped"
            {
              nativeBuildInputs = [
                pkgs.bubblewrap
              ];
              buildInputs = [
                cardano-node.packages.${system}.cardano-node
                cardano-node.packages.${system}.cardano-cli
                hydra.packages.${system}.hydra-node
              ];
            }
            ''
              mkdir -p $out/log
              exec &> >(tee $out/log/test.log)
              bwrap \
                --ro-bind /nix/store /nix/store \
                --bind /build /build \
                --share-net \
                --proc /proc \
                --ro-bind ${pkgs.tzdata}/share/zoneinfo /usr/share/zoneinfo \
                -- ${test}/bin/${test.exeName} >&2
            '';

          hydraChecks = haskellNixFlake.packages // {
            # NOTE: mind that we use `packages` here, not checks
            hydra-test = wrapTest haskellNixFlake.packages."hydra-auction:test:hydra-auction-test";
          };
        in
        rec {
          inherit haskellNixFlake;
          packages = {
            default = haskellNixFlake.packages."hydra-auction:exe:hydra-auction";
            # FIXME: this can probably be removed
            check = pkgs.runCommand "combined-test"
              {
                nativeBuildInputs = builtins.attrValues self.checks.${system};
              } "touch $out";
            cliImage = pkgs.dockerTools.buildLayeredImage
              {
                name = "hydra-auction-cli";
                tag = "latest";
                contents = [ haskellNixFlake.packages."hydra-auction:exe:hydra-auction" ];
                config = {
                  Cmd = [ "hydra-auction" ];
                };
              };
            delegateImage = pkgs.dockerTools.buildLayeredImage
              {
                name = "hydra-auction-delegate";
                tag = "latest";
                contents = [ haskellNixFlake.packages."hydra-auction:exe:hydra-auction-delegate" ];
                config = {
                  Cmd = [ "hydra-auction-delegate" ];
                };
              };
            platformImage = pkgs.dockerTools.buildLayeredImage
              {
                name = "hydra-auction-platform";
                tag = "latest";
                contents = [ haskellNixFlake.packages."hydra-auction:exe:hydra-auction-platform" ];
                config = {
                  Cmd = [ "hydra-auction-platform" ];
                };
              };
          };

          devShells = builtins.mapAttrs
            (_: shell: shell.overrideAttrs (old: {
              shellHook = old.shellHook + preCommitHook.shellHook;
              buildInputs = old.buildInputs ++ [
                pkgs.docker-compose
                pkgs.jq
              ];
            }))
            haskellNixFlake.devShells;

          checks = hydraChecks // {
            formatting = preCommitHook;
          };

        }) // {
        inherit removeIncompatibleAttrs;
        herculesCI.ciSystems = [ "x86_64-linux" ];
      });
}
