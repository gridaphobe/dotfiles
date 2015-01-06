{ melpa, fetchFromGitHub, async }:

melpa.mkDerivation (self: {
  pname   = "helm";
  version = "20150105";

  src = fetchFromGitHub {
    owner  = "emacs-helm";
    repo   = self.pname;
    rev    = "e5608ad86e7ca72446a4b1aa0faf604200ffe895";
    sha256 = "0n2kr6pyzcsi8pq6faxz2y8kicz1gmvj98fzzlq3a107dqqp25ay";
  };
  
  packageRequires = [ async ];

})
