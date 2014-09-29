{ stdenv, fetchgit, emacs, texinfo }:

stdenv.mkDerivation {
  name = "haskell-mode-3ca8c10";

  src = fetchgit {
    url = "git://github.com/haskell/haskell-mode.git";
    rev = "3ca8c1097a3a270cd7dcf77d7109b47550337979";
    sha256 = "2debd11403719df103f196face4eb9d8934a97f5e086e8f51d1f88b95bd232cf";
  };
  
  buildInputs = [ emacs texinfo ];

  installPhase = ''
    mkdir -p "$out/share/emacs/site-lisp" "$out/share/info"
    cp *.el *.elc *.hs "$out/share/emacs/site-lisp/"
    cp haskell-mode.info "$out/share/info/"
  '';

}

