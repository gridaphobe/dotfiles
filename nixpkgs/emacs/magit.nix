{ melpa, fetchFromGitHub, git-commit-mode, git-rebase-mode }:

melpa.mkDerivation (self: {
  pname   = "magit";
  version = "20141025";

  src = fetchFromGitHub {
    owner  = "magit";
    repo   = "magit";
    rev    = "50c08522c8a3c67e0f3b821fe4df61e8bd456ff9";
    sha256 = "0mzyx72pidzvla1x2qszn3c60n2j0n8i5k875c4difvd1n4p0vsk";
  };

  packageRequires = [ git-commit-mode git-rebase-mode ];

})
