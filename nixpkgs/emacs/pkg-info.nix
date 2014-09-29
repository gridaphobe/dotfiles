{ stdenv, fetchgit, emacs, texinfo, epl }:

stdenv.mkDerivation {
  name = "pkg-info-475cdeb";
  src = fetchgit {
    url = "git://github.com/lunaryorn/pkg-info.el.git";
    rev = "475cdeb0b8d44f9854e506c429eeb445787014ec";
    sha256 = "c9f1a59aafddce9766838e1a26e375cd6afc7db296267853dba925938014ddb7";
  };
  
  buildInputs = [ emacs texinfo epl ];
  
  buildPhase = ''
    emacs --batch -Q -L . -L ${epl}/share/emacs/site-lisp pkg-info.el
  '';
  
  installPhase = ''
    mkdir -p "$out/share/emacs/site-lisp"
    cp *.el *.elc "$out/share/emacs/site-lisp/"
  '';
}
