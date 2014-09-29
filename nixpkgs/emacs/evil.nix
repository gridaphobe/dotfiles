{ stdenv, fetchgit, emacs, texinfo, undoTree }:

stdenv.mkDerivation {
  name = "evil-4382854";
  src = fetchgit {
    url = "git://gitorious.org/evil/evil";
    rev = "438285437a9acd7edd99a95f52c1ab0fbb0a6107";
    sha256 = "18736fc8100801ab1198b33fbf0f08cf4a3779dd80771dfa3f700e7182ac9f6d";
  };
  
  buildInputs = [ emacs texinfo undoTree ];
  
  installPhase = ''
    mkdir -p "$out/share/emacs/site-lisp"
    cp *.el *.elc "$out/share/emacs/site-lisp/"
  '';
}
