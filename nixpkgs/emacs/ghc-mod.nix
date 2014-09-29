{ stdenv, fetchgit, emacs, texinfo, ghcMod }:

stdenv.mkDerivation {
  name = "ghc-mod-el-${ghcMod.version}";
  src = ghcMod.src;

  buildInputs = [ emacs texinfo ];

  preConfigure = ''
    cd elisp
  '';
  
  installPhase = ''
    mkdir -p "$out/share/emacs/site-lisp"
    cp *.el *.elc "$out/share/emacs/site-lisp/"
  '';
}
