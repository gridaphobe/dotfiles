{ stdenv, fetchgit, emacs, texinfo, dash }:

stdenv.mkDerivation {
  name = "smartparens-1a67c9b";
  src = fetchgit {
    url = "git://github.com/Fuco1/smartparens.git";
    rev = "1a67c9b73104c36725e5e3d7798e144a3758d324";
    sha256 = "a653484cb8aef0b18ae603b43e9778a6cd624201745644ca3ee4eab8aa200d10";
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
