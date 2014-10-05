{ stdenv, fetchgit, emacs, texinfo, mu }:

stdenv.mkDerivation {
  name = "emacs-mu4e-maildirs-extension-c78263a";
  src = fetchgit {
    url = "git://github.com/agpchil/mu4e-maildirs-extension";
    rev = "c78263aff6b2a72877f511a13e5909b27fbb8d63";
    sha256 = "64a893fcf7c9ed7a85d0e7ff293c54eb6f78f1c75bc7b63db2f5373d3b58108f";
  };
  
  buildInputs = [ emacs texinfo mu ];
  
  buildPhase = ''
    emacs --batch -Q -L . -L ${mu}/share/emacs/site-lisp/mu4e epl.el
  '';
  
  installPhase = ''
    mkdir -p "$out/share/emacs/site-lisp"
    cp *.el *.elc "$out/share/emacs/site-lisp/"
  '';
}
