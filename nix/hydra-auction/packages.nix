# A set of buildables we typically build for releases

{ myProject # as defined in default.nix
, hydra
}:
let

  nativePkgs = myProject.hsPkgs;
in
rec {

  hydra-auction-error =
    nativePkgs.hydra-auction-error.components.library;

  hydra-auction-onchain =
    nativePkgs.hydra-auction-onchain.components.library;

  # tests = {
  #   hydra-auction-plutus =
  #     wrapTest nativePkgs.hydra-auction-plutus.components.tests.tests;
  # };

}
