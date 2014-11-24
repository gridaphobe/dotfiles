{ melpa, fetchFromGitHub, dash-el }:

melpa.mkDerivation (self: {
  pname   = "smartparens";
  version = "1.6.2";

  src = fetchFromGitHub {
    owner  = "Fuco1";
    repo   = self.pname;
    rev    = self.version;
    sha256 = "16pzd740vd1r3qfmxia2ibiarinm6xpja0mjv3nni5dis5s4r9gc";
  };

  packageRequires = [ dash-el ];

})
