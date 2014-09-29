{ stdenv, fetchgit, emacs, texinfo }:

stdenv.mkDerivation {
  name = "epl-63c78c0";
  src = fetchgit {
    url = "git://github.com/cask/epl.git";
    rev = "63c78c08e345455f3d4daa844fdc551a2c18024e";
    sha256 = "b8e8c121e33f06a03abac0208a0beee278c41043f24d9d013d3d3333570471de";
  };
  
  buildInputs = [ emacs texinfo ];
  
  buildPhase = ''
    emacs --batch -Q -L . epl.el
  '';
  
  installPhase = ''
    mkdir -p "$out/share/emacs/site-lisp"
    cp *.el *.elc "$out/share/emacs/site-lisp/"
  '';
}
