{ melpa, fetchFromGitHub, dash-el, pkg-info-el }:

melpa.mkDerivation (self: {
  pname   = "flycheck";
  version = "0.20";

  src = fetchFromGitHub {
    owner  = self.pname;
    repo   = self.pname;
    rev    = self.version;
    sha256 = "0cq7y7ssm6phvx5pfv2yqq4j0yqmm0lhjav7v4a8ql7094cd790a";
  };
  
  packageRequires = [ dash-el pkg-info-el ];

})
