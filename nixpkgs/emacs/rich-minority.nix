{ stdenv, fetchgit, emacs, texinfo, dash }:

stdenv.mkDerivation {
  name = "rich-minority-a3611ad";
  src = fetchgit {
    url = "git://github.com/Bruce-Connor/rich-minority.git";
    rev = "a3611adff20491d60d93b2d6284b1291fcf351dc";
    sha256 = "9855703798194e7bf50954ee931b4c31ba96c396ee5f0e68ab122884b0c7fc2b";
  };
  buildInputs = [ emacs texinfo dash ];

  buildPhase = ''
    emacs --batch -Q -L . -L ${dash}/share/emacs/site-lisp -f batch-byte-compile *.el
  '';

  installPhase = ''
    mkdir -p "$out/share/emacs/site-lisp"
    cp *.el *.elc "$out/share/emacs/site-lisp/"
  '';
}
