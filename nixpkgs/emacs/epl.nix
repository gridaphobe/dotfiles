{ melpa, fetchFromGitHub }:

melpa.mkDerivation (self: {
  pname   = "epl";
  version = "20140823";

  src = fetchFromGitHub {
    owner  = "cask";
    repo   = self.pname;
    rev    = "63c78c08e345455f3d4daa844fdc551a2c18024e";
    sha256 = "04a2aq8dj2cmy77vw142wcmnjvqdbdsp6z0psrzz2qw0b0am03li";
  };

})
