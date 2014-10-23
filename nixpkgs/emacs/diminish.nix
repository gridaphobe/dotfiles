{ melpa, fetchFromGitHub }:

melpa.mkDerivation (self: {
  pname   = "diminish";
  version = "0.44";

  src = fetchFromGitHub {
    owner  = "emacsmirror";
    repo   = self.pname;
    rev    = self.version;
    sha256 = "0hshw7z5f8pqxvgxw74kbj6nvprsgfvy45fl854xarnkvqcara09";
  };
  
})
