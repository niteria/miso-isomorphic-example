let

  bootstrap = import <nixpkgs> {};

  misoTarball = bootstrap.fetchFromGitHub {
    owner = "dmjio";
    repo = "miso";
    rev = "25ffd607f27f8282676e7000f7e311d0af0e6188";
    sha256 = "06sqwwxzq2lwg2qiqri2hl2v70dkl6sr391035m8fqy9b4acalhf";
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
