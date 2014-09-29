{ stdenv, fetchurl, emacs, texinfo }:

stdenv.mkDerivation {
  name = "undo-tree-0.6.4";
  src = fetchurl {
    url = "http://www.dr-qubit.org/download.php?file=undo-tree/undo-tree-0.6.4.el";
    sha256 = "00fja17ibdgyjwfm2a4j7gx7f7m4hxjnmwiaz4452xcwcbwhqbz9";
  };
  
  buildInputs = [ emacs texinfo ];
  
  unpackPhase = ''
    cp $src undo-tree.el
  '';
  
  buildPhase = ''
    emacs --batch -Q -L . -f batch-byte-compile undo-tree.el
  '';
  
  installPhase = ''
    mkdir -p "$out/share/emacs/site-lisp"
    cp *.el *.elc "$out/share/emacs/site-lisp/"
  '';
}
