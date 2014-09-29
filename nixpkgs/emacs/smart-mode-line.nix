{ stdenv, fetchgit, emacs, texinfo, dash, richMinority }:

stdenv.mkDerivation {
  name = "smart-mode-line-79834194";
  src = fetchgit {
    url = "git://github.com/Bruce-Connor/smart-mode-line.git";
    rev = "79834194ef8b708a3957cc7aec3fc9b21ddc7937";
    sha256 = "be2727bcd3ea4da0270348dfbb33d7f7c1adf12d6d165f0663e5c11e98ac252d";
  };
  buildInputs = [ emacs texinfo dash richMinority ];

  buildPhase = ''
    emacs --batch -Q -L . -L ${dash}/share/emacs/site-lisp -L ${richMinority}/share/emacs/site-lisp -f batch-byte-compile *.el
  '';

  installPhase = ''
    mkdir -p "$out/share/emacs/site-lisp"
    cp *.el *.elc "$out/share/emacs/site-lisp/"
  '';
}
