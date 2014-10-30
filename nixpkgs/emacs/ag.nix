{ melpa, fetchFromGitHub, dash-el, s-el }:

melpa.mkDerivation (self: {
  pname   = "ag";
  version = "0.44";

  src = fetchFromGitHub {
    owner  = "Wilfred";
    repo   = "${self.pname}.el";
    rev    = self.version;
    sha256 = "19y5w9m2flp4as54q8yfngrkri3kd7fdha9pf2xjgx6ryflqx61k";
  };

  packageRequires = [ dash-el s-el ];

})
