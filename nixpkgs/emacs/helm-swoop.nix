{ melpa, fetchFromGitHub, helm }:

melpa.mkDerivation (self: {
  pname   = "helm-swoop";
  version = "20141224";

  src = fetchFromGitHub {
    owner  = "ShingoFukuyama";
    repo   = self.pname;
    rev    = "06a251f7d7fce2a5719e0862e5855972cd8ab1ae";
    sha256 = "0nq33ldhbvfbm6jnsxqdf3vwaqrsr2gprkzll081gcyl2s1x0l2m";
  };
  
  packageRequires = [ helm ];

})
