{ melpa, fetchurl}:

melpa.mkDerivation (self: {
  pname   = "org-plus-contrib";
  version = "20141020";

  src = fetchurl {
    url    = "http://orgmode.org/elpa/${self.fname}.tar";
    sha256 = "02njxmdbmias2f5psvwqc115dyakcwm2g381gfdv8qz4sqav0r77";
  };
  
  buildPhase = ''
    cp $src .
  '';
})
