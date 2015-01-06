{ melpa, fetchFromGitHub }:

melpa.mkDerivation (self: {
  pname   = "haskell-mode";
  version = "20150101";

  src = fetchFromGitHub {
    owner  = "haskell";
    repo   = self.pname;
    rev    = "0db5efaaeb3b22e5a3fdafa600729e14c1716ee2";
    sha256 = "0d63cgzj579cr8zbrnl0inyy35b26sxinqxr7bgrjsngpmhm52an";
  };

})
