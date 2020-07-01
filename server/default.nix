let
  pkgs = import ../nixpkgs.nix;
  inherit (pkgs.haskell.packages) ghc865;
  server = ghc865.callCabal2nixWithOptions "miso-isomorphic-example" ../. "-fProd" { };
  reload-script = pkgs.writeScriptBin "reload" ''
    ${ghc865.ghcid}/bin/ghcid -c \
      '${ghc865.cabal-install}/bin/cabal new-repl -fProd --write-ghc-environment-files never' \
      -W -s ':load Main' --run=':main'
  '';
  serverEnv = server.env.overrideAttrs (old: {
    buildInputs = old.buildInputs ++ [reload-script ghc865.cabal-install ghc865.hie];
  });
in if pkgs.lib.inNixShell then serverEnv else server
