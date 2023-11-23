{
  inputs = {
    hydra.url = "github:input-output-hk/hydra/9f1027e0fdff6765f5233f19c4639fdaa3558bfa";

    nixpkgs.follows = "hydra/nixpkgs";
    haskellNix.follows = "hydra/haskellNix";
    iohk-nix.follows = "hydra/iohk-nix";
    flake-utils.follows = "hydra/flake-utils";
    CHaP.follows = "hydra/CHaP";
    cardano-node.follows = "hydra/cardano-node";

  };

  outputs =
    { self
    , flake-utils
    , nixpkgs
    , cardano-node
    , hydra
    , ...
    } @ inputs:
    flake-utils.lib.eachSystem [
      "x86_64-linux"
    ]
      (system:
      let
        pkgs = import inputs.nixpkgs { inherit system; };
        myProject = import ./nix/hydra-auction/project.nix {
          inherit (inputs) haskellNix iohk-nix CHaP;
          inherit system nixpkgs;
        };
        myPackages = import ./nix/hydra-auction/packages.nix {
          inherit myProject system pkgs cardano-node hydra;
        };
        myImages = import ./nix/hydra-auction/docker.nix {
          inherit myPackages system nixpkgs;
        };
        prefixAttrs = s: attrs:
          with pkgs.lib.attrsets;
          mapAttrs' (name: value: nameValuePair (s + name) value) attrs;
      in
      rec {
        inherit myProject;

        packages =
          { default = myPackages.hydra-auction-offchain; } //
          myPackages //
          prefixAttrs "docker-" myImages;

        devShells = (import ./nix/hydra-auction/shell.nix {
          inherit (inputs) cardano-node hydra;
          inherit myProject system;
        }) // {
          ci = (import ./nix/hydra-auction/shell.nix {
            inherit (inputs) cardano-node hydra;
            inherit myProject system;
            withoutDevTools = true;
          }).default;
        };

      });

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
  };
}
