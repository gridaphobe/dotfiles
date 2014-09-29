{ stdenv, fetchgit, emacs, texinfo, dash, helm, s-el, pkg-info, epl }:

stdenv.mkDerivation {
  name = "projectile-8d6b17f";
  src = fetchgit {
    url = "git://github.com/bbatsov/projectile.git";
    rev = "8d6b17fbe78c238c0e8d5a81d2bba805c0b18151";
    sha256 = "4cfc91310391e058cf51aa9a504418bf90f3b22be6af4ff8279e185344533248";
  };
  
  buildInputs = [ emacs texinfo dash helm s-el pkg-info epl ];
  
  buildPhase = ''
    emacs --batch -Q -L . -L ${dash}/share/emacs/site-lisp -L ${helm}/share/emacs/site-lisp \
          -L ${s-el}/share/emacs/site-lisp -L ${pkg-info}/share/emacs/site-lisp \
          -L ${epl}/share/emacs/site-lisp \
          -f batch-byte-compile projectile.el helm-projectile.el
  '';
  
  installPhase = ''
    mkdir -p "$out/share/emacs/site-lisp"
    cp *.el *.elc "$out/share/emacs/site-lisp/"
  '';
}
