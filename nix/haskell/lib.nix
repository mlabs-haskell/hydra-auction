{ lib
  # e.g. "x86_64-linux"
, system # : string
, indexState ? null # : string
, iohkNixCryptoOverlay # : overlay
, iohkNixHaskellNixCryptoOverlay # : overlay
, haskellNixNixpkgs # : nixpkgs
, haskellNixOverlay # : overlay
}:

let
  pkgs = import haskellNixNixpkgs {
    inherit system;
    overlays = [
      iohkNixCryptoOverlay
      iohkNixHaskellNixCryptoOverlay
      haskellNixOverlay
    ];
  };
in

{ name # : string
, src # : path
  # e.g. ghc928
, ghcVersion # : string
, haskellModules ? [ ]
, externalDependencies ? [ ]
, externalRepositories ? { }
}:
let
  mkHackage = pkgs.callPackage ./mk-hackage.nix {
    nix-tools = pkgs.haskell-nix.nix-tools-set {
      compiler-nix-name = ghcVersion;
    };
  };

  # This looks like a noop but without it haskell.nix throws a runtime
  # error about `pkgs` attribute not being present which is nonsense
  # https://input-output-hk.github.io/haskell.nix/reference/library.html?highlight=cabalProject#modules
  fixedHaskellModules = map (m: args @ { ... }: m args) haskellModules;

  flatExternalDependencies =
    lib.lists.concatMap
      (dep: [ (dep.passthru or { }).src or dep ] ++
        (flatExternalDependencies (dep.passthru or { }).externalDependencies or [ ]));

  flattenedExternalDependencies = flatExternalDependencies externalDependencies;

  customHackages = mkHackage {
    srcs = map toString flattenedExternalDependencies;
    inherit name;
  };

  project = pkgs.haskell-nix.cabalProject' {
    inherit src;
    name = name;
    index-state = indexState;

    compiler-nix-name = ghcVersion;
    inputMap = lib.mapAttrs (_: toString) externalRepositories;

    modules = customHackages.modules ++ fixedHaskellModules;
    inherit (customHackages) extra-hackages extra-hackage-tarballs;

    shell = {
      withHoogle = true;
      # exactDeps = true;

      tools = {
        cabal = { };
        haskell-language-server = { };
      };
    };
  };

  projectFlake = project.flake { };

  augmentedPackages = builtins.mapAttrs
    (_: package:
      package // {
        passthru = (package.passthru or { }) // {
          inherit src externalDependencies;
        };
      })
    (projectFlake.packages or { });
in
projectFlake // {
  packages = augmentedPackages;
}
