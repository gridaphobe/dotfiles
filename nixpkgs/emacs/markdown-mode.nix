{ melpa, fetchFromGitHub }:

melpa.mkDerivation (self: {
  pname   = "markdown-mode";
  version = "2.0";

  src = fetchFromGitHub {
    owner  = "defunkt";
    repo   = self.pname;
    rev    = "v${self.version}";
    sha256 = "1l2w0j9xl8pipz61426s79jq2yns42vjvysc6yjc29kbsnhalj29";
  };

})
