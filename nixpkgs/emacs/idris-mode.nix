{ melpa, fetchFromGitHub, flycheck }:

melpa.mkDerivation (self: {
  pname   = "idris-mode";
  version = "0.9.15";

  src = fetchFromGitHub {
    owner  = "idris-hackers";
    repo   = "idris-mode";
    rev    = self.version;
    sha256 = "00pkgk1zxan89i8alsa2dpa9ls7imqk5zb1kbjwzrlbr0gk4smdb";
  };

  packageRequires = [ flycheck ];

})
