{ melpa, fetchurl}:

melpa.mkDerivation (self: {
  pname   = "org-plus-contrib";
  version = "20141020";

  src = fetchurl {
    url    = "http://orgmode.org/elpa/${self.fname}.tar";
    sha256 = "02njxmdbmias2f5psvwqc115dyakcwm2g381gfdv8qz4sqav0r77";
  };
  
  buildPhase   = "true";
  installPhase = ''
    emacs --batch -q --eval \
      "(progn (setq package-user-dir \"$out/share/emacs/site-lisp/elpa\") \
              (package-initialize)
              (package-install-file \"$src\")
       )"
  '';

})
