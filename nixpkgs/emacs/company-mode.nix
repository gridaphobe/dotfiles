{ melpa, fetchFromGitHub }:

melpa.mkDerivation (self: {
  pname   = "company-mode";
  version = "0.8.6";

  src = fetchFromGitHub {
    owner  = "company-mode";
    repo   = "company-mode";
    rev    = self.version;
    sha256 = "1xwxyqg5dan8m1qkdxyzm066ryf24h07karpdlm3s09izfdny33f";
  };
  
})
