{ melpa, fetchFromGitHub }:

melpa.mkDerivation (self: {
  pname   = "async";
  version = "1.2";

  src = fetchFromGitHub {
    owner  = "jwiegley";
    repo   = "emacs-async";
    rev    = "v${self.version}";
    sha256 = "1j6mbvvbnm2m1gpsy9ipxiv76b684nn57yssbqdyiwyy499cma6q";
  };
  
})
