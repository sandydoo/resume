{ pkgs, stdenv, git, texutils, create-resume }:

let
  fonts = pkgs.makeFontsConf { fontDirectories = with pkgs; [ inter ibm-plex ]; };

  inherit (pkgs.lib) hasSuffix;
  inherit (pkgs.lib.filesystem) listFilesRecursive;
  tex-env = texutils.lib.callTex2Nix {
    inherit pkgs;
    srcs = builtins.filter (p: hasSuffix ".tex" p) (listFilesRecursive ./templates);
    extraTexPackages = { inherit (pkgs.texlive) scheme-minimal latexmk; };
  };
in
stdenv.mkDerivation {
  name = "resume";
  version = "0.0.1";
  src = ./.;

  buildInputs = with pkgs; [
    git
    tex-env
    create-resume
  ];

  preBuild = ''
    export FONTCONFIG_FILE=${fonts}
  '';

  buildPhase = ''
    runHook preBuild
    ${create-resume}/bin/create-resume --template templates/cv.tex me.json
  '';

  installPhase = ''
    mkdir -p $out
    cp cv.pdf $out/cv.pdf
  '';
}
