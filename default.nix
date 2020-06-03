let

  pkgs = import ./nixpkgs.nix;
  server = pkgs.callPackage ./server/default.nix { inherit pkgs; };
  client = pkgs.callPackage ./client/default.nix { inherit pkgs; };

in

  pkgs.runCommand "miso-ismorphic-example" { inherit client server; } ''
    mkdir -p $out/{bin,static}
    cp ${server}/bin/* $out/bin/
    cp ${client}/static/* $out/static/
  ''

