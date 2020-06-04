let

  bootstrap = import <nixpkgs> {};

  misoTarball = bootstrap.fetchFromGitHub {
    owner = "dmjio";
    repo = "miso";
    rev = "70bf9aa9aec0467ca551c366a3e4c19f99db6a02";
    sha256 = "0rms9wf21xqs6ij4r9yd391ppxwwdx3si7nhrhqq25r0l29k0w2k";
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
