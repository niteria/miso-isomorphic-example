{ jsaddle ? true }:
let
  pkgs = import ../nixpkgs.nix;
  inherit (pkgs.haskell.packages) ghc865 ghcjs86;
  client = ghcjs86.callCabal2nix "miso-isomorphic-example" ../. { };
  client-jsaddle = ghc865.callCabal2nix "miso-isomorphic-example" ../. {
    miso = ghc865.miso-jsaddle;
  };
  reload-script = pkgs.writeScriptBin "reload" ''
    ${ghc865.ghcid}/bin/ghcid -c \
      '${ghc865.cabal-install}/bin/cabal new-repl client --write-ghc-environment-files never' \
      -W -s ':load Main' --run=':main'
  '';
  build-script = pkgs.writeScriptBin "build" ''
    set -ex
    rm -rf static/all.js
    mkdir -p static
    ${ghc865.cabal-install}/bin/cabal new-build --ghcjs --write-ghc-environment-files never
    cp dist-newstyle/build/x86_64-linux/ghcjs-*/miso-isomorphic-example-*/x/client/build/client/client.jsexe/all.js static/
  '';
  client-jsaddleEnv = client-jsaddle.env.overrideAttrs (old: {
    buildInputs = old.buildInputs ++ [reload-script ghc865.cabal-install ghc865.hie];
  });
  clientEnv = client.env.overrideAttrs (old: {
    buildInputs = old.buildInputs ++ [build-script ghc865.cabal-install ghc865.hie];
  });
in if pkgs.lib.inNixShell then
  if jsaddle then client-jsaddleEnv
  else clientEnv
else client
