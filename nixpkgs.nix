let

  bootstrap = import <nixpkgs> {};

  misoTarball = bootstrap.fetchFromGitHub {
    owner = "niteria";
    repo = "miso";
    rev = "e4f311307afc740eef8c0c2e11139ad87e33df27";
    sha256 = "18bm1m3131bmk8m3jl04w3pbwwkigibchmlcd4pjnclsd2ya99zd";
  };

  miso = import "${misoTarball}" { } ;

  inherit (miso) pkgs;

  pkgsWithGHC = pkgs // {
    haskell = pkgs.haskell // {
      packages = pkgs.haskell.packages // {
        ghc = pkgs.haskell.packages.ghc865;
        ghcjs = pkgs.haskell.packages.ghcjs86;
      };
    };
  };

in pkgsWithGHC
