{ melpa, fetchFromGitHub, evil, god-mode }:

melpa.mkDerivation (self: {
  pname   = "evil-god-state";
  version = "20140830";

  src = fetchFromGitHub {
    owner  = "gridaphobe";
    repo   = self.pname;
    rev    = "234a9b6f500ece89c3dfb5c1df5baef6963e4566";
    sha256 = "16v6dpw1hibrkf9hga88gv5axvp1pajd67brnh5h4wpdy9qvwgyy";
  };
  
  packageRequires = [ evil god-mode ];

})
