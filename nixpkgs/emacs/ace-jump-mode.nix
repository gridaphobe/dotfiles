{ melpa, fetchFromGitHub }:

melpa.mkDerivation (self: {
  pname   = "ace-jump-mode";
  version = "20140616";

  src = fetchFromGitHub {
    owner  = "winterTTr";
    repo   = self.pname;
    rev    = "8351e2df4fbbeb2a4003f2fb39f46d33803f3dac";
    sha256 = "17axrgd99glnl6ma4ls3k01ysdqmiqr581wnrbsn3s4gp53mm2x6";
  };

})
