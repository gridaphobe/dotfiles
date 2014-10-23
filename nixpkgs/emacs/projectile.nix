{ melpa, fetchFromGitHub, dash-el, helm, s-el, pkg-info-el, epl }:

melpa.mkDerivation (self: {
  pname   = "projectile";
  version = "20141020";

  src = fetchFromGitHub {
    owner  = "bbatsov";
    repo   = self.pname;
    rev    = "13580d83374e0c17c55b3a680b816dfae407657e";
    sha256 = "10c28h2g53sg68lwamhak0shdhh26h5xaipipz3n4281sr1fwg58";
  };

  packageRequires = [ dash-el helm s-el pkg-info-el epl ];

})
