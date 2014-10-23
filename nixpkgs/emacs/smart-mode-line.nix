{ melpa, fetchFromGitHub, dash-el, rich-minority }:

melpa.mkDerivation (self: {
  pname   = "smart-mode-line";
  version = "2.6";

  src = fetchFromGitHub {
    owner  = "Bruce-Connor";
    repo   = self.pname;
    rev    = self.version;
    sha256 = "17nav2jbvbd13xzgp29x396mc617n2dh6whjk4wnyvsyv7r0s9f6";
  };
  
  packageRequires = [ dash-el rich-minority ];

})
