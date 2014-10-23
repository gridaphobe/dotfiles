{ melpa, fetchFromGitHub }:

melpa.mkDerivation (self: {
  pname   = "switch-window";
  version = "20140919";

  src = fetchFromGitHub {
    owner  = "dimitri";
    repo   = self.pname;
    rev    = "3ffbe68e584f811e891f96afa1de15e0d9c1ebb5";
    sha256 = "09221128a0f55a575ed9addb3a435cfe01ab6bdd0cca5d589ccd37de61ceccbd";
  };

})
