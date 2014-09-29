{ stdenv, fetchgit, emacs, texinfo }:

stdenv.mkDerivation {
  name = "exec-path-from-shell-8748769";
  src = fetchgit {
    url = "git://github.com/purcell/exec-path-from-shell.git";
    rev = "8748769a6c64dc7496c9967244c177c6b3977e65";
    sha256 = "6984dc5eed71272a456437161569fa167d214a410704e606510579e4f03dd32e";
  };
  
  buildInputs = [ emacs texinfo ];
  
  buildPhase = ''
    emacs --batch -Q -L . exec-path-from-shell.el
  '';
  
  installPhase = ''
    mkdir -p "$out/share/emacs/site-lisp"
    cp *.el *.elc "$out/share/emacs/site-lisp/"
  '';
}
