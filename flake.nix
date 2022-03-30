{
  description = "";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-21.11";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.tex2nix-flake.url = "github:Mic92/tex2nix";
  inputs.tex2nix-flake.inputs.utils.follows = "nixpkgs";

  outputs = { self, nixpkgs, flake-utils, tex2nix-flake }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
        };

        ghc = pkgs.ghc;

        tex-env = import ./nix/tex-env.nix { texlive = pkgs.texlive; };
        tex2nix = import tex2nix-flake { inherit pkgs; };
      in
      rec {
        defaultPackage = ghc.callPackage ./nix/resume.nix { };

        devShell = pkgs.mkShell {
          packages = with pkgs; [
            ghc
            nixpkgs-fmt
            cabal2nix
            haskell-language-server
            stylish-haskell
            zlib.dev
            tex2nix
            tex-env
            inter
            ibm-plex
          ];

          # inputsFrom = [ defaultPackage ];
        };
      }
    );
}
