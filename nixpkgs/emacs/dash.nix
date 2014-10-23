{ melpa, fetchFromGitHub }:

melpa.mkDerivation (self: {
  pname   = "dash";
  version = "2.9.0";

  src = fetchFromGitHub {
    owner  = "magnars";
    repo   = "${self.pname}.el";
    rev    = self.version;
    sha256 = "1lg31s8y6ljsz6ps765ia5px39wim626xy8fbc4jpk8fym1jh7ay";
  };

})
