{ lib
, flake-parts-lib
, ...
}:
let
  inherit (flake-parts-lib) mkPerSystemOption;
  inherit (lib) types mkOption;
  configName = "chap";
in
{
  options = {
    perSystem = mkPerSystemOption ({ config, pkgs, ... }: {
      options = {
        ${configName} = lib.mkOption {
          type = types.attrsOf (types.submodule ({ ... }: {
            options = {
              src = mkOption {
                type = types.path;
              };

              ghcVersion = mkOption {
                type = types.str;
                example = "ghc945";
              };

              cardanoPackages = mkOption {
                type = types.nullOr types.package;
                default = null;
              };

              externalDependencies = mkOption {
                type = types.listOf (types.oneOf [ types.str types.package ]);
                default = [ ];
              };
            };
          }));
          default = { };
        };
        libChap = lib.mkOption {
          type = lib.types.anything;
          default = { };
        };
      };
      config =
        let
          mkChapPackage = pkgs.callPackage ./lib.nix { };
          projects =
            lib.attrsets.mapAttrs
              (config.libUtils.withNameAttr mkChapPackage)
              config.${configName};
        in
        {
          haskell = projects;
          libChap = {
            mkPackage = mkChapPackage;
          };
        };
    });
  };
}
