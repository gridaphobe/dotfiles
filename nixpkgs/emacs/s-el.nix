{ stdenv, fetchgit, emacs, texinfo, }:

stdenv.mkDerivation {
  name = "s.el-1f85b51";
  src = fetchgit {
    url = "git://github.com/magnars/s.el.git";
    rev = "1f85b5112f3f68169ddaa2911fcfa030f979eb4d";
    sha256 = "9d871ea84f98c51099528a03eddf47218cf70f1431d4c35c19c977d9e73d421f";
  };
  
  buildInputs = [ emacs texinfo ];
  
  buildPhase = ''
    emacs --batch -Q -L . s.el
  '';
  
  installPhase = ''
    mkdir -p "$out/share/emacs/site-lisp"
    cp *.el *.elc "$out/share/emacs/site-lisp/"
  '';
}
