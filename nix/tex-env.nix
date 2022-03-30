# Generated with tex2nix 0.0.0
{ texlive, extraTexPackages ? {} }:
(texlive.combine ({
    inherit (texlive) scheme-small;
    "tracklang" = texlive."tracklang";
    "xkeyval" = texlive."xkeyval";
    "kvsetkeys" = texlive."kvsetkeys";
    "rerunfilecheck" = texlive."rerunfilecheck";
    "ltxcmds" = texlive."ltxcmds";
    "graphics" = texlive."graphics";
    "datetime2" = texlive."datetime2";
    "auxhook" = texlive."auxhook";
    "uniquecounter" = texlive."uniquecounter";
    "intcalc" = texlive."intcalc";
    "hyperref" = texlive."hyperref";
    "tipa" = texlive."tipa";
    "enumitem" = texlive."enumitem";
    "xcolor" = texlive."xcolor";
    "infwarerr" = texlive."infwarerr";
    "atbegshi" = texlive."atbegshi";
    "parskip" = texlive."parskip";
    "ntheorem" = texlive."ntheorem";
    "etexcmds" = texlive."etexcmds";
    "url" = texlive."url";
    "etoolbox" = texlive."etoolbox";
    "kvdefinekeys" = texlive."kvdefinekeys";
    "iftex" = texlive."iftex";
    "titlesec" = texlive."titlesec";
    "hopatch" = texlive."hopatch";
    "fancyhdr" = texlive."fancyhdr";
    "gettitlestring" = texlive."gettitlestring";
    "atveryend" = texlive."atveryend";
    "refcount" = texlive."refcount";
    "kvoptions" = texlive."kvoptions";
    "bitset" = texlive."bitset";
    "setspace" = texlive."setspace";
    "minitoc" = texlive."minitoc";
    "letltxmacro" = texlive."letltxmacro";
    "geometry" = texlive."geometry";
    "pdfescape" = texlive."pdfescape";
    "fontspec" = texlive."fontspec";
    "hycolor" = texlive."hycolor";
    "pdftexcmds" = texlive."pdftexcmds";

} // extraTexPackages))
