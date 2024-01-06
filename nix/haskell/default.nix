{ self
, lib
, flake-parts-lib
, ...
}:
let
  inherit (flake-parts-lib) mkPerSystemOption;
  inherit (lib) types mkOption;
  configName = "haskell";
in
{
  options = {
    perSystem = mkPerSystemOption ({ config, system, ... }: {
      options = {
        ${configName} = lib.mkOption {
          type = types.attrsOf (types.submodule ({ ... }: {
            options = {
              # ignored
              name = mkOption {
                type = types.string;
                default = "";
                internal = true;
              };

              src = mkOption {
                type = types.path;
              };

              ghcVersion = mkOption {
                type = types.str;
                example = "ghc928";
              };

              externalDependencies = mkOption {
                type = types.listOf (types.oneOf [ types.str types.package ]);
                default = [ ];
              };

              haskellModules = mkOption {
                type = types.listOf types.anything;
                default = [ ];
              };

              externalRepositories = mkOption {
                type = types.attrsOf (types.oneOf [ types.str types.package ]);
                default = { };
              };

            };
          }));
        };

        libHaskell = mkOption {
          type = types.anything;
          default = { };
        };
      };

      config =
        let
          mkHaskellPackage = import ./lib.nix {
            inherit lib system;
            iohkNixCryptoOverlay = self.inputs.iohk-nix.overlays.crypto;
            iohkNixHaskellNixCryptoOverlay = self.inputs.iohk-nix.overlays.haskell-nix-crypto;
            haskellNixNixpkgs = self.inputs.haskell-nix.inputs.nixpkgs;
            haskellNixOverlay = self.inputs.haskell-nix.overlay;
          };

          projects =
            lib.attrsets.mapAttrs
              (config.libUtils.withNameAttr mkHaskellPackage)
              config.${configName};

          flat2With = mkName: xs:
            builtins.listToAttrs
              (lib.flatten
                (lib.mapAttrsToList
                  (nameSpace: attrs:
                    lib.mapAttrsToList
                      (attrName: value: {
                        name = mkName nameSpace attrName;
                        inherit value;
                      })
                      attrs)
                  xs));


          getAttrs = attr: flat2With (a: b: "${a}:${b}")
            (lib.mapAttrs
              (_: project: project.${attr} or { })
              projects);

          getAttr = attr: (lib.mapAttrs
            (_: project: project.${attr})
            projects);

        in
        {
          packages = getAttrs "packages";
          devShells = getAttr "devShell";
          checks = getAttrs "checks";
          apps = getAttrs "apps";

          libHaskell = {
            mkPackage = mkHaskellPackage;
          };
        };
    });
  };
}
