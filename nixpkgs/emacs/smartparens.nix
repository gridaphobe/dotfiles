{ melpa, fetchFromGitHub, dash-el }:

melpa.mkDerivation (self: {
  pname   = "smartparens";
  version = "20141009";

  src = fetchFromGitHub {
    owner  = "Fuco1";
    repo   = self.pname;
    rev    = "ea55c2b7ef66887a6f5383a00e142f6fecfe24a0";
    sha256 = "0nf05qr2qfbxgj15qff94qqknnfi7xas54198m8cgiaysd81dsci";
  };
  
  packageRequires = [ dash-el ];

})
