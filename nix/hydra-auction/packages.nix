# A set of buildables we typically build for releases

{ myProject # as defined in default.nix
, system ? builtins.currentSystem
, pkgs
, cardano-node
, hydra
}:
let
  lib = pkgs.lib;

  nativePkgs = myProject.hsPkgs;

  myComponents = nativePkgs.hydra-auction.components;

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
in
rec {
  hydra-auction = myComponents.exes.hydra-auction;

  hydra-auction-delegate = myComponents.exes.hydra-auction-delegate;
  
  hydra-auction-platform = myComponents.exes.hydra-auction-platform;

  tests = {
    hydra-auction = wrapTest myComponents.tests.hydra-auction-test;
  };

}
