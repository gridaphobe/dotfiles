{ stdenv, fetchgit, emacs, texinfo }:

stdenv.mkDerivation {
  name = "helm-2c5ab7d";
  src = fetchgit {
    url = "git://github.com/emacs-helm/helm.git";
    rev = "2c5ab7de1d3084422ce062c999e27d96384e8808";
    sha256 = "4195fa646d2829cb7d72fd37f67c75b826f954d868d6c0a2d5b5ff0a5fc6628d";
  };
  
  buildInputs = [ emacs texinfo ];
  
  installPhase = ''
    mkdir -p "$out/share/emacs/site-lisp"
    cp *.el *.elc "$out/share/emacs/site-lisp/"
  '';
}
