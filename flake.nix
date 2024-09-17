{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    h-raylib.url = "github:Anut-py/h-raylib";
  };

  outputs = { self, nixpkgs, flake-utils, h-raylib }:
    flake-utils.lib.eachDefaultSystem
      (system:
        let
          pkgs = (import nixpkgs { inherit system; });
        in
        {
          devShell = ((pkgs.haskellPackages.extend (final: prev: {
            h-raylib = h-raylib.packages.${system}.default;
          })).extend (pkgs.haskell.lib.compose.packageSourceOverrides {
            pong = ./.;
          })).shellFor {
            packages = p: [
              p.pong
            ];
            withHoogle = false;
            buildInputs = [ pkgs.cabal-install ];
          };
          formatter = pkgs.nixpkgs-fmt;
        }
      );
}
 
