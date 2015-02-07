{ melpa, fetchFromGitHub }:

melpa.mkDerivation (self: {
  pname   = "wgrep";
  version = "20141017";

  src = fetchFromGitHub {
    owner  = "mhayashi1120";
    repo   = "Emacs-wgrep";
    rev    = "7ef26c51feaef8a5ec0929737130ab8ba326983c";
    sha256 = "075z0glain0dp56d0cp468y5y88wn82ab26aapsrdzq8hmlshwn4";
  };
  
})
