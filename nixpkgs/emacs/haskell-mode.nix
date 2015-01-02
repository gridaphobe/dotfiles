{ melpa, fetchFromGitHub }:

melpa.mkDerivation (self: {
  pname   = "haskell-mode";
  version = "20141206";

  src = fetchFromGitHub {
    owner  = "haskell";
    repo   = self.pname;
    rev    = "0dd5055dd922a960906950ae9bedba705c0cd6d0";
    sha256 = "07swamlhwiqyj78mbk62hsa5sxldpld506k7nky7mc2dcdx8pf7j";
  };

})
