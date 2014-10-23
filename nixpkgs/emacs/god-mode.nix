{ melpa, fetchFromGitHub }:

melpa.mkDerivation (self: {
  pname   = "god-mode";
  version = "20140811";

  src = fetchFromGitHub {
    owner  = "chrisdone";
    repo   = self.pname;
    rev    = "6b7ae259a58ca1d7776aa4eca9f1092e4c0033e6";
    sha256 = "1amr98nq82g2d3f3f5wlqm9g38j64avygnsi9rrlbfqz4f71vq7x";
  };

})
