let

  bootstrap = import <nixpkgs> {};

  misoTarball = bootstrap.fetchFromGitHub {
    owner = "dmjio";
    repo = "miso";
    rev = "936ce48eb8ec33024de8c5ea8d1832654a41e595";
    sha256 = "192jnwlv6saf7glrxv87a619by61j56szpzhh552s57zg276nhai";
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
