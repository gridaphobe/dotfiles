{ melpa, fetchurl}:

melpa.mkDerivation (self: {
  pname   = "auctex";
  version = "11.87.7";

  src = fetchurl {
    url    = "http://elpa.gnu.org/packages/${self.fname}.tar";
    sha256 = "07bhw8zc3d1f2basjy80njmxpsp4f70kg3ynkch9ghlai3mm2b7n";
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
