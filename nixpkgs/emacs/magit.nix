{ melpa, fetchFromGitHub, git-commit-mode, git-rebase-mode }:

melpa.mkDerivation (self: {
  pname   = "magit";
  version = "20141016";

  src = fetchFromGitHub {
    owner  = "magit";
    repo   = "magit";
    rev    = "90141016";
    sha256 = "11d3gzj0hlb7wqsjzjb0vf9i0ik4xzwdyayjy4hfgx0gjmymkfx3";
  };

  packageRequires = [ git-commit-mode git-rebase-mode ];

})
