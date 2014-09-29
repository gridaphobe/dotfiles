{ stdenv, fetchgit, emacs, texinfo }:

stdenv.mkDerivation {
  name = "use-package-a3ac75f";
  src = fetchgit {
    url = "git://github.com/jwiegley/use-package.git";
    rev = "a3ac75ff82680864cde1a7091c5427004ae6238b";
    sha256 = "178e532f7faeb1a3463ab1a22ee7e28ca4ed769d619a3af18ad0236378758ee6";
  };
  
  buildInputs = [ emacs texinfo ];
  
  buildPhase = ''
     emacs --batch -Q -L . -f batch-byte-compile bind-key.el use-package.el
  '';
  
  installPhase = ''
    mkdir -p "$out/share/emacs/site-lisp"
    cp *.el *.elc "$out/share/emacs/site-lisp/"
  '';
}
