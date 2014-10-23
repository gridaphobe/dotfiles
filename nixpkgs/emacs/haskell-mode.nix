{ melpa, fetchFromGitHub }:

melpa.mkDerivation (self: {
  pname   = "haskell-mode";
  version = "20141022";

  src = fetchFromGitHub {
    owner  = "haskell";
    repo   = self.pname;
    rev    = "cd8fe8197e08527990cdbd732475f0f7a2e134f2";
    sha256 = "0jnwicsns8g9bj3a76mllw1a6a5mpbdffbmx2jlw6kgib1c4kdgz";
  };

})
