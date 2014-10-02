{ stdenv, fetchgit, emacs, texinfo, evil, godMode }:

stdenv.mkDerivation {
  name = "evil-god-state-234a9b6";
  src = fetchgit {
    url = "git://github.com/gridaphobe/evil-god-state";
    rev = "234a9b6f500ece89c3dfb5c1df5baef6963e4566";
    sha256 = "de3fbe71f2ed72020bb4791dd3a4bae1eeaeca7e08a907939b794518f86d669b";
  };
  buildInputs = [ emacs texinfo evil godMode ];

  buildPhase = ''
    emacs --batch -Q -L . -L ${evil}/share/emacs/site-lisp -L ${godMode}/share/emacs/site-lisp -f batch-byte-compile *.el
  '';

  installPhase = ''
    mkdir -p "$out/share/emacs/site-lisp"
    cp *.el *.elc "$out/share/emacs/site-lisp/"
  '';
}
