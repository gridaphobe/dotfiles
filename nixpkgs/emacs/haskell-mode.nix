{ melpa, fetchFromGitHub }:

melpa.mkDerivation (self: {
  pname   = "haskell-mode";
  version = "20141104";

  src = fetchFromGitHub {
    owner  = "haskell";
    repo   = self.pname;
    rev    = "d618e44ed4684261cc0a4e4c317048d7dc43a4a1";
    sha256 = "12lqnf7z2nhgglf3gkd5wqk8z0qc16ybh3ipzdfqbna59d20yiza";
  };

})
