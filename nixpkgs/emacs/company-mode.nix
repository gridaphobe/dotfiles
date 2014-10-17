{ stdenv, fetchgit, emacs, texinfo }:

stdenv.mkDerivation {
  name = "company-mode-0.8.5";
  src = fetchgit {
    url = "git://github.com/company-mode/company-mode.git";
    rev = "fa4ba155a3e22ddc4b8bc33fcbf8cc69ef8f0043";
    sha256 = "063f6c5595ba112ec9c9bb7d09a2f7453c8b427cee3a822dfac0cc1de5452cac";
  };
  
  buildInputs = [ emacs texinfo ];
  
  buildPhase = ''
    rm company-yasnippet.el
    make compile
  '';
  
  installPhase = ''
    mkdir -p "$out/share/emacs/site-lisp"
    cp *.el *.elc "$out/share/emacs/site-lisp/"
  '';
}
