{ melpa, fetchFromGitHub }:

melpa.mkDerivation (self: {
  pname   = "volatile-highlights";
  version = "1.11";

  src = fetchFromGitHub {
    owner  = "k-talo";
    repo   = "${self.pname}.el";
    rev    = "fb2abc2d4d4051a9a6b7c8de2fe7564161f01f24";
    sha256 = "1v0chqj5jir4685jd8ahw86g9zdmi6xd05wmzhyw20rbk924fcqf";
  };

})
