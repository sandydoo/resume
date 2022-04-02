{
  description = "";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-21.11";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.tex2nix-flake.url = "github:Mic92/tex2nix";
  inputs.tex2nix-flake.inputs.utils.follows = "nixpkgs";
  inputs.texutils.url = "github:Ninlives/texutils.nix";

  outputs = { self, nixpkgs, flake-utils, tex2nix-flake, texutils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
        };

        ghc = pkgs.haskell.compiler.ghc8107;

        tex2nix = import tex2nix-flake { inherit pkgs; };

        git = pkgs.writeShellScriptBin "git" ''
          echo "${self.shortRev or "dirty"}"
        '';
      in
      rec {
        packages.resume = pkgs.callPackage ./resume.nix {
          inherit pkgs;
          inherit texutils;
          inherit git;
          create-resume = packages.create-resume;
        };

        packages.create-resume = pkgs.haskellPackages.callPackage ./create-resume.nix { };
        defaultPackage = packages.resume;

        devShell = pkgs.mkShell {
          packages = with pkgs; [
            ghc
            nixpkgs-fmt
            cabal2nix
            haskell-language-server
            stylish-haskell
            zlib.dev
            tex2nix
          ];

          # inputsFrom = [ packages.resume ];
        };
      }
    );
}
