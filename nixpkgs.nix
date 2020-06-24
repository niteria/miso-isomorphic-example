let

  bootstrap = import <nixpkgs> {};

  misoTarball = bootstrap.fetchFromGitHub {
    owner = "niteria";
    repo = "miso";
    rev = "02dd2a3734e1cb6cfb29c97d81660d06aff10dba";
    sha256 = "09mkc71f0qzfaxrd7zxxlx57yy8bxrvq79ixax343bczxy9hr1aq";
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
