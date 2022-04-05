{ pkgs, stdenv, git, fonts, tex-env, create-resume }:

stdenv.mkDerivation {
  name = "resume";
  src = ./.;

  FONTCONFIG_FILE = fonts;

  buildInputs = with pkgs; [
    git
    tex-env
    create-resume
  ];

  buildPhase = ''
    ${create-resume}/bin/create-resume --template templates/cv.tex me.json
  '';

  installPhase = ''
    mkdir -p $out
    cp cv.pdf $out/cv.pdf
  '';
}
