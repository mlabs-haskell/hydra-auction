{
  description = "Hydra Auction";

  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix/0.0.64";
    iohkNix.url = "github:input-output-hk/iohk-nix/d31417fe8c8fbfb697b3ad4c498e17eb046874b9";
    chap = {
        url = "github:input-output-hk/cardano-haskell-packages/695c91a740abfeef0860056227c605abf6375edd";
        flake = false;
    };
    nixpkgs.follows = "haskellNix/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = inputs@{ self, haskellNix, iohkNix, chap, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system overlays;
          inherit (haskellNix) config;
        };

        cabalProject = inputs.hydra-auction.cabalProject {
          inputMap = { "https://input-output-hk.github.io/cardano-haskell-packages" = chap; };
        };

        overlays = [
          haskellNix.overlay

          # needed for cardano-api which uses a patched libsodium  
          iohkNix.overlays.crypto

          (final: prev: {
            hydraAuctionProject = final.haskell-nix.project' {
              compiler-nix-name = "ghc8107";

              src = final.haskell-nix.haskellLib.cleanGit {
                name = "hydra-auction";
                src = ./.;
              };

              
              modules = [{
                # https://github.com/input-output-hk/iohk-nix/pull/488
                packages.cardano-crypto-class.components.library.pkgconfig = final.lib.mkForce [ [ final.libsodium-vrf final.secp256k1 ] ];
                packages.cardano-crypto-praos.components.library.pkgconfig = final.lib.mkForce [ [ final.libsodium-vrf ] ];
              }];

              shell = {
                nativeBuildInputs = with final; [
                  nixpkgs-fmt
                  git
                ];

                tools = {
                  cabal = { };
                  cabal-fmt = { };
                  fourmolu = "0.4.0.0";
                };
              };
            };
          })
        ];

        flake = pkgs.hydraAuctionProject.flake { };
        exe-component-name = "hydra-auction:exe:hydra-auction";

      in
      flake // {
        defaultPackage = flake.packages.${exe-component-name};
        defaultApp = flake.apps.${exe-component-name};
        check = pkgs.runCommand "combined-test"
          {
            nativeBuildInputs = builtins.attrValues flake.checks;
          } "touch $out";
      });
}