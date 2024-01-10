# NOTE: It does not return a package, but a package specification to be built
# by the Haskell module

{ fetchFromGitHub
}:

let
  defaultCardanoPackages = fetchFromGitHub {
    owner = "input-output-hk";
    repo = "cardano-haskell-packages";
    rev = "380718ca9fb94980fa53f8b6f35be59e4b6aa2aa";
    sha256 = "sha256-NsUtW5mDu3osqBFVBL1ilq1fPgdFLRLdso6d/nAK7o8=";
  };
in

args:
let
  cardanoPackages =
    if args.cardanoPackages or null == null
    then defaultCardanoPackages
    else args.cardanoPackages;

in
{
  inherit (args) name src ghcVersion;

  externalRepositories = {
    "https://input-output-hk.github.io/cardano-haskell-packages" = cardanoPackages;
  };

  externalDependencies = [
  ] ++ (args.externalDependencies or [ ]);
}
