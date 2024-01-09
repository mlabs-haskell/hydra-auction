{
  perSystem = { pkgs, config, ... }:
    let
      # Have fun keeping them in sync

      cardanoPackages = pkgs.fetchFromGitHub {
        owner = "input-output-hk";
        repo = "cardano-haskell-packages";
        rev = "3a758410d00a426ce51596d4bc236d1b1d388b12";
        hash = "sha256-RkuWkTcWufRer/fyzcAnvJA7WLvNitdG8JMKSaNDJag=";
      };

      atlas = pkgs.fetchFromGitHub {
        owner = "geniusyield";
        repo = "atlas";
        rev = "v0.4.0";
        hash = "sha256-V/+6Eodwzm5uf9h+hBhxTUd12DEPZG/MmPcDessluAo=";
      };

      blockfrost-haskell = pkgs.fetchFromGitHub {
        owner = "blockfrost";
        repo = "blockfrost-haskell";
        rev = "206e1a0f62e2a7cc08d05db8d62cef8dc2fbd98e";
        hash = "sha256-R6BP3hwrOBmlRabA3prUuOGkYzETmQIM+K+Oh+fczEY=";
      };

      cardano-wallet = pkgs.fetchFromGitHub {
        owner = "cardano-foundation";
        repo = "cardano-wallet";
        rev = "v2023-07-18";
        hash = "sha256-ijflgIw+1FpLoxM4Rksf4MJvNqnEPAv3gNWE8zMuefU=";
      };

      cardano-addresses = pkgs.fetchFromGitHub {
        owner = "IntersectMBO";
        repo = "cardano-addresses";
        rev = "6b55f96d57a181f898eb2a50531d3ae4280c549c";
        hash = "sha256-7vhGs/uGAueok/Df6D/e8KKQtln9OWgmV3XEklJVz3s=";
      };

      plutus-simple-model = pkgs.fetchFromGitHub {
        owner = "geniusyield";
        repo = "plutus-simple-model";
        rev = "0cb63af903a835c73aec662092eb67d228bba9b0";
        hash = "sha256-H56EyRFNdDvLDo9FVeGZyQZ92itQPG39TkMVyEC/xqM=";
      };

      maestro-sdk = pkgs.fetchFromGitHub {
        owner = "maestro-org";
        repo = "haskell-sdk";
        rev = "37fc9d4297e7a8e2fda51f9396bc69232b2ba5f9";
        hash = "sha256-X2fDXSfILfRvRoV25KaDRxKfhzbJyn8m3n9VbEcODNs=";
      };

      hydra-auction = config.libHaskell.mkPackage {
        name = "hydra-auction";
        src = ./.;
        ghcVersion = "ghc928";
        externalDependencies = [
          atlas
          "${blockfrost-haskell}/blockfrost-api"
          "${blockfrost-haskell}/blockfrost-client-core"
          "${blockfrost-haskell}/blockfrost-client"
          "${cardano-wallet}/lib/balance-tx"
          "${cardano-wallet}/lib/coin-selection"
          "${cardano-wallet}/lib/delta-store"
          "${cardano-wallet}/lib/delta-table"
          "${cardano-wallet}/lib/delta-types"
          "${cardano-wallet}/lib/launcher"
          "${cardano-wallet}/lib/numeric"
          "${cardano-wallet}/lib/primitive"
          "${cardano-wallet}/lib/test-utils"
          "${cardano-wallet}/lib/text-class"
          "${cardano-wallet}/lib/wai-middleware-logging"
          "${cardano-wallet}/lib/wallet"
          "${cardano-wallet}/lib/wallet-benchmarks"
          "${cardano-addresses}/core"
          "${plutus-simple-model}/psm"
          "${plutus-simple-model}/cardano-simple"
          maestro-sdk
        ];

        externalRepositories = {
          "https://input-output-hk.github.io/cardano-haskell-packages" = cardanoPackages;
        };
      };
    in
    {
      devShells.hydra-auction = hydra-auction.devShell;

      packages.hydra-auction-error = hydra-auction.packages."hydra-auction-error:lib:hydra-auction-error";
      packages.hydra-auction-offchain = hydra-auction.packages."hydra-auction-offchain:lib:hydra-auction-offchain";
      packages.hydra-auction-onchain = hydra-auction.packages."hydra-auction-onchain:lib:hydra-auction-onchain";
      packages.hydra-auction-onchain-compiled = hydra-auction.packages."hydra-auction-onchain:lib:hydra-auction-onchain-compiled";

    };
}
