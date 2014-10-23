{ melpa, fetchFromGitHub }:

melpa.mkDerivation (self: {
  pname   = "exec-path-from-shell";
  version = "20141022";

  src = fetchFromGitHub {
    owner  = "purcell";
    repo   = self.pname;
    rev    = "e4af0e9b44738e7474c89ed895200b42e6541515";
    sha256 = "0lxikiqf1jik88lf889q4f4f8kdgg3npciz298x605nhbfd5snbd";
  };

})
