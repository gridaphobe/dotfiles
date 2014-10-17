{ stdenv, fetchgit, emacs, texinfo, s-el }:

stdenv.mkDerivation {
  name = "emacs-weechat-el-715a1cd";
  src = fetchgit {
    url = "git://github.com/the-kenny/weechat.el";
    rev = "715a1cd16ee4d1fc81f333bc4c3ff54517f8688a";
    sha256 = "8ffd71945a384d592ceb4ae3a74fd74c98a56fb28157963a114efe65445d3a3e";
  };
  buildInputs = [ emacs texinfo s-el ];

  buildPhase = ''
    rm weechat-sauron.el weechat-secrets.el weechat-smiley.el weechat-tracking.el
    emacs --batch -Q -L . -L ${s-el}/share/emacs/site-lisp -f batch-byte-compile *.el
  '';

  installPhase = ''
    install -d "$out/share/emacs/site-lisp"
    install *.el *.elc "$out/share/emacs/site-lisp/"
  '';
}
