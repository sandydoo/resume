{
  description = "";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-21.11";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
        };
        ghc = pkgs.ghc;
      in
      rec {
        defaultPackage = ghc.callPackage ./default.nix { };

        devShell = pkgs.mkShell {
          packages = with pkgs; [
            ghc
            nixpkgs-fmt
            cabal2nix
            haskell-language-server
            stylish-haskell
            zlib.dev
          ];

          # inputsFrom = [ defaultPackage ];
        };
      }
    );
}
