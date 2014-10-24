{ melpa, fetchFromGitHub }:

melpa.mkDerivation (self: {
  pname   = "popup";
  version = "0.5.0";

  src = fetchFromGitHub {
    owner  = "auto-complete";
    repo   = "${self.pname}-el";
    rev    = "v${self.version}";
    sha256 = "0836ayyz1syvd9ry97ya06l8mpr88c6xbgb4d98szj6iwbypcj7b";
  };
  
})
