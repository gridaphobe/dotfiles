{ stdenv, fetchgit, emacs, texinfo }:

stdenv.mkDerivation {
  name = "dash-b1e585d";
  src = fetchgit {
    url = "git://github.com/magnars/dash.el";
    rev = "b1e585dcddef4d8fb9305a6d34f7c1b18079a81e";
    sha256 = "a471e0f599f9b569bdfc179886419b2c37c8a931335d3417ecbb6687717fea02";
  };
  buildInputs = [ emacs texinfo ];

  buildPhase = ''
    emacs --batch -Q -L . -f batch-byte-compile *.el
  '';

  installPhase = ''
    mkdir -p "$out/share/emacs/site-lisp"
    cp *.el *.elc "$out/share/emacs/site-lisp/"
  '';
}
