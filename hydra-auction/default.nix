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


      hydra-auction = config.libHaskell.mkPackage {
        name = "hydra-auction";
        src = ./.;
        ghcVersion = "ghc928";
        externalDependencies = [
        ];

        externalRepositories = {
          "https://input-output-hk.github.io/cardano-haskell-packages" = cardanoPackages;
        };
      };
    in
    {
      apps.hydra-auction-onchain = hydra-auction.packages."hydra-auction-onchain:exe:hydra-auction-onchain";

      devShells.hydra-auction = hydra-auction.devShell;

      packages.hydra-auction-error = hydra-auction.packages."hydra-auction-error:lib:hydra-auction-error";
      packages.hydra-auction-onchain = hydra-auction.packages."hydra-auction-onchain:lib:hydra-auction-onchain";

    };
}
