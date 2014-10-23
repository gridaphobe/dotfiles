{ melpa, fetchFromGitHub }:

melpa.mkDerivation (self: {
  pname   = "bind-key";
  version = "20141013";

  src = fetchFromGitHub {
    owner  = "jwiegley";
    repo   = "use-package";
    rev    = "d43af5e0769a92f77e01dea229e376d9006722ef";
    sha256 = "1m4v5h52brg2g9rpbqfq9m3m8fv520vg5mjwppnbw6099d17msqd";
  };
  
  files = [ "bind-key.el" ];

})
