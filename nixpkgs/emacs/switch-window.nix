{ stdenv, fetchgit, emacs, texinfo }:

stdenv.mkDerivation {
  name = "emacs-switch-window-3ffbe68";
  src = fetchgit {
    url = "git://github.com/dimitri/switch-window.el";
    rev = "3ffbe68e584f811e891f96afa1de15e0d9c1ebb5";
    sha256 = "09221128a0f55a575ed9addb3a435cfe01ab6bdd0cca5d589ccd37de61ceccbd";
  };
  buildInputs = [ emacs texinfo ];

  buildPhase = ''
    emacs --batch -Q -L . -f batch-byte-compile *.el
  '';

  installPhase = ''
    install -d "$out/share/emacs/site-lisp"
    install *.el *.elc "$out/share/emacs/site-lisp/"
  '';
}
