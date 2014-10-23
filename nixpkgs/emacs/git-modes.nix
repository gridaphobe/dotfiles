{ stdenv, fetchgit, emacs, texinfo }:

stdenv.mkDerivation {
  name = "git-modes-42e989c";
  src = fetchgit {
    url = "git://github.com/magit/git-modes.git";
    rev = "42e989c178aa0f90cedf9e6221dcbf888a9db2b0";
    sha256 = "6b6815034180c698df6ccdbcabea173c7ba9c2fa60a24a28bec6854544e290b6";
  };

  buildInputs = [ emacs texinfo ];

  buildPhase = ''
    make lisp
  '';

  checkPhase = ''
    make test
  '';
  doCheck = true;

  installPhase = ''
    emacs --batch -q --eval \
      "(progn (setq package-user-dir \"$out/share/emacs/site-lisp/elpa\") \
              (package-initialize) \
              (package-install-file \"git-commit-mode.el\") \
              (package-install-file \"git-rebase-mode.el\") \
              (package-install-file \"gitattributes-mode.el\") \
              (package-install-file \"gitconfig-mode.el\") \
              (package-install-file \"gitignore-mode.el\") \
       )"
  '';
}
