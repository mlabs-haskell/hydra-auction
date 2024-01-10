{ mkHaskellPackage
, fetchFromGitHub
}:

(mkHaskellPackage {
  name = "fourmolu";
  src = fetchFromGitHub {
    owner = "fourmolu";
    repo = "fourmolu";
    rev = "v0.14.0.0";
    hash = "sha256-Df6mo8pZwNWO1tmAR13bm5lM/2r4XH+hiJKofOQAN/Y=";
  };

  ghcVersion = "ghc928";
}).packages."fourmolu:exe:fourmolu"
