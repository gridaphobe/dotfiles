{ melpa, fetchFromGitHub }:

melpa.mkDerivation (self: {
  pname   = "s";
  version = "20140910";

  src = fetchFromGitHub {
    owner  = "magnars";
    repo   = "${self.pname}.el";
    rev    = "1f85b5112f3f68169ddaa2911fcfa030f979eb4d";
    sha256 = "9d871ea84f98c51099528a03eddf47218cf70f1431d4c35c19c977d9e73d421f";
  };

})
