{
  description = "My resume";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-22.05";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.tex2nix-flake.url = "github:Mic92/tex2nix";
  inputs.tex2nix-flake.inputs.utils.follows = "nixpkgs";
  inputs.texutils.url = "github:Ninlives/texutils.nix";

  outputs = { self, nixpkgs, flake-utils, tex2nix-flake, texutils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };

        ghc = pkgs.haskell.compiler.ghc8107;

        tex2nix = import tex2nix-flake { inherit pkgs; };

        # The .git folder isn’t available in flakes, so we create a fake git
        # command that returns the current revision.
        git = pkgs.writeShellScriptBin "git" ''
          echo "${self.shortRev or "dirty"}"
        '';
      in
      rec {
        packages.fonts = pkgs.makeFontsConf {
          fontDirectories = with pkgs; [ inter ibm-plex ];
        };

        packages.tex-env =
          let
            inherit (pkgs.lib) hasSuffix;
            inherit (pkgs.lib.filesystem) listFilesRecursive;
          in
          texutils.lib.callTex2Nix {
            inherit pkgs;
            srcs = builtins.filter (p: hasSuffix ".tex" p) (listFilesRecursive ./templates);
            extraTexPackages = { inherit (pkgs.texlive) scheme-minimal latexmk; };
          };

        packages.resume = pkgs.callPackage ./resume.nix {
          inherit pkgs git;
          inherit (packages) fonts tex-env create-resume;
        };

        packages.create-resume = pkgs.haskellPackages.callPackage ./create-resume.nix { };
        defaultPackage = packages.resume;

        devShell = pkgs.mkShell {
          FONTCONFIG_FILE = packages.fonts;

          packages = with pkgs; [
            ghc
            nixpkgs-fmt
            cabal2nix
            haskell-language-server
            stylish-haskell
            zlib.dev
            tex2nix
            packages.tex-env
          ];
        };
      }
    );
}
