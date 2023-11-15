# A shell setup providing build tools and utilities for a development
# environment. The main shell environment is based on haskell.nix and uses the
# same nixpkgs as the default nix builds (see default.nix).

{
  # Used in CI to have a smaller closure
  withoutDevTools ? false
, myProject
, cardano-node
, hydra
, system ? builtins.currentSystem
}:
let
  inherit (myProject) compiler pkgs hsPkgs;

  cabal = pkgs.haskell-nix.cabal-install.${compiler};

  # haskell-language-server = pkgs.haskell-nix.tool compiler "haskell-language-server" rec {
  #   src = pkgs.haskell-nix.sources."hls-1.10";
  #   cabalProject = builtins.readFile (src + "/cabal.project");
  #   sha256map."https://github.com/pepeiborra/ekg-json"."7a0af7a8fd38045fd15fb13445bdcc7085325460" = "sha256-fVwKxGgM0S4Kv/4egVAAiAjV7QB5PBqMVMCfsv7otIQ=";
  # };

  libs = [
    pkgs.glibcLocales
    pkgs.libsodium-vrf # from iohk-nix overlay
    pkgs.lzma
    pkgs.secp256k1
    pkgs.zlib
  ]
  ++
  pkgs.lib.optionals (pkgs.stdenv.isLinux) [ pkgs.systemd ];

  buildInputs = [
    # Build essentials
    pkgs.git
    pkgs.pkgconfig
    cabal
    pkgs.haskellPackages.hspec-discover
    pkgs.haskellPackages.cabal-plan
    # For validating JSON instances against a pre-defined schema
    pkgs.check-jsonschema
    # For generating plantuml drawings
    pkgs.plantuml
    # For plotting results of hydra-cluster benchmarks
    pkgs.gnuplot
  ] ++ pkgs.lib.optionals (!pkgs.stdenv.isDarwin) [
    # For integration tests
    cardano-node.packages.${system}.cardano-node
  ];

  devInputs = if withoutDevTools then [ ] else [
    # Essenetial for a good IDE
    # haskell-language-server
    # Automagically format .hs and .cabal files
    pkgs.haskellPackages.fourmolu
    pkgs.haskellPackages.cabal-fmt
    # The interactive Glasgow Haskell Compiler as a Daemon
    pkgs.haskellPackages.ghcid
    # Generate a graph of the module dependencies in the "dot" format
    pkgs.haskellPackages.graphmod
    # Handy to interact with the hydra-node via websockets
    pkgs.websocat
    # Like 'jq' to manipulate JSON, but work for YAML
    pkgs.yq
    # For docs/ (i.e. Docusaurus, Node.js & React)
    pkgs.yarn
    pkgs.nodejs
  ] ++ pkgs.lib.optionals (!pkgs.stdenv.isDarwin) [
    # To interact with cardano-node and testing out things
    cardano-node.packages.${system}.cardano-cli
  ];

  haskellNixShell = hsPkgs.shellFor {
    # NOTE: Explicit list of local packages as hoogle would not work otherwise.
    # Make sure these are consistent with the packages in cabal.project.
    packages = ps: with ps; [
      hydra-auction-plutus
    ];

    buildInputs = libs ++ buildInputs ++ devInputs;

    withHoogle = false;
    withHaddock = false;

    # Always create missing golden files
    CREATE_MISSING_GOLDEN = 1;

    # Force a UTF-8 locale because many Haskell programs and tests
    # assume this.
    LANG = "en_US.UTF-8";
    LC_ALL = "en_US.UTF-8";

    GIT_SSL_CAINFO = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";

    shellHook = ''
      command -v cardano-node || echo "WARNING: 'cardano-node' not found"
      command -v cardano-cli  || echo "WARNING: 'cardano-cli' not found"
    '';
  };

in
{
  default = haskellNixShell;
}
