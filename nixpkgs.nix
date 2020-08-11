let

  bootstrap = import <nixpkgs> {};

  misoTarball = bootstrap.fetchFromGitHub {
    owner = "dmjio";
    repo = "miso";
    rev = "ea25964565074e73d4052b56b60b6e101fa08bc5";
    sha256 = "1yb9yvc0ln4yn1jk2k5kwwa1s32310abawz40yd8cqqkm1z7w6wg";
  };

  servant-jsaddle = bootstrap.fetchFromGitHub {
    owner = "haskell-servant";
    repo = "servant-jsaddle";
    rev = "2ccf13d185e26d4cb4a51622e748ec64336435f4";
    sha256 = "066vr1rfq6bjn3xx9g52z2vgp1ibyz50z3hzwaqq3fzxnr2srpjs";
  };

  all-hies = import (bootstrap.fetchFromGitHub {
    owner = "Infinisil";
    repo = "all-hies";
    rev = "4b6aab017cdf96a90641dc287437685675d598da";
    sha256 = "0ap12mbzk97zmxk42fk8vqacyvpxk29r2wrnjqpx4m2w9g7gfdya";
  }) {};

  hie = all-hies.selection { selector = p : { inherit (p) ghc865; }; };

  ghc865 = (pkgs.haskell.packages.ghc865.override {
    all-cabal-hashes = pkgs.fetchurl {
      url = "https://github.com/commercialhaskell/all-cabal-hashes/archive/8c7bdd9ad4bc3671b4214e32766873d443af2622.tar.gz";
      sha256 = "0q9qdpvn3c64rwnafcqkzzyi4z72mvvwmvn06d89fnzfpqjxvwx2";
    };
  }).extend (self: super: {
      clay = self.callHackage "clay" "0.13.3" {};
      http-proxy = pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.doJailbreak (self.callHackage "http-proxy" "0.1.1.0" {}));
      servant-client-core = self.callHackage "servant-client-core" "0.16" {};
      servant = self.callHackage "servant" "0.16" {};
      servant-server = self.callHackage "servant-server" "0.16" {};
      servant-lucid = self.callHackage "servant-lucid" "0.9" {};
      servant-jsaddle = pkgs.haskell.lib.dontCheck (self.callCabal2nix "servant-jsaddle" servant-jsaddle {});
      hie = hie;
    }
  );

  ghcjs86 = (pkgs.haskell.packages.ghcjs86.override {
    all-cabal-hashes = pkgs.fetchurl {
      url = "https://github.com/commercialhaskell/all-cabal-hashes/archive/8c7bdd9ad4bc3671b4214e32766873d443af2622.tar.gz";
      sha256 = "0q9qdpvn3c64rwnafcqkzzyi4z72mvvwmvn06d89fnzfpqjxvwx2";
    };
  }).extend (self: super: {
      servant-client-core = self.callHackage "servant-client-core" "0.16" {};
      servant = self.callHackage "servant" "0.16" {};
      servant-jsaddle = pkgs.haskell.lib.dontCheck (ghcjs86.callCabal2nix "servant-jsaddle" servant-jsaddle {});
    }
  );

  miso = import "${misoTarball}" { nixpkgsConfig = { allowBroken = true; } ; } ;

  inherit (miso) pkgs;

  pkgsWithGHC = pkgs // {
    haskell = pkgs.haskell // {
      packages = pkgs.haskell.packages // {
        ghc865 = ghc865;
        ghcjs86 = ghcjs86;
      };
    };
  };

in pkgsWithGHC
