{ melpa, fetchFromGitHub }:

melpa.mkDerivation (self: {
  pname   = "expand-region";
  version = "20141012";

  src = fetchFromGitHub {
    owner  = "magnars";
    repo   = "${self.pname}.el";
    rev    = "fa413e07c97997d950c92d6012f5442b5c3cee78";
    sha256 = "04k0518wfy72wpzsswmncnhd372fxa0r8nbfhmbyfmns8n7sr045";
  };

})
