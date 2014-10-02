{ stdenv, fetchgit, emacs, texinfo }:

stdenv.mkDerivation {
  name = "god-mode-6b7ae25";
  src = fetchgit {
    url = "git://github.com/chrisdone/god-mode";
    rev = "6b7ae259a58ca1d7776aa4eca9f1092e4c0033e6";
    sha256 = "d20b4d859ed40475bfe7ee9f71e8b61fa38bd8c2831b55e871f3c225211c7014";
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
