{ stdenv, fetchgit, emacs, texinfo }:

stdenv.mkDerivation {
  name = "git-modes-345d32c";
  src = fetchgit {
    url = "git://github.com/magit/git-modes.git";
    rev = "345d32c0f7ef5dd4b91236677cab52ef0010dcac";
    sha256 = "664eb42d8a20b39e75ae85493e434ff86c1f82c30861fa1a88f50a41c7cb6758";
  };
  
  buildInputs = [ emacs texinfo ];
  
  buildPhase = ''
    make lisp
  '';
  
  installPhase = ''
    mkdir -p "$out/share/emacs/site-lisp"
    cp *.el *.elc "$out/share/emacs/site-lisp/"
  '';
}
