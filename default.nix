let

  pkgs = import ./nixpkgs.nix;
  inherit (pkgs) runCommand closurecompiler;
  inherit (pkgs.haskell.packages) ghcjs86 ghc865;
  server = import ./server/default.nix;
  client = import ./client/default.nix { jsaddle = false; };

in

  pkgs.runCommand "miso-ismorphic-example" { inherit client server; } ''
    mkdir -p $out/{bin,static}
    cp ${server}/bin/* $out/bin/
    ${closurecompiler}/bin/closure-compiler --compilation_level ADVANCED_OPTIMIZATIONS \
      --jscomp_off=checkVars \
      --externs=${client}/bin/client.jsexe/all.js.externs \
      ${client}/bin/client.jsexe/all.js > temp.js
    mv temp.js $out/static/all.js
  ''

