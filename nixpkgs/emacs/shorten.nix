{ melpa, fetchFromGitHub }:

melpa.mkDerivation (self: {
  pname   = "shorten";
  version = "1.5";

  src = fetchFromGitHub {
    owner  = "jorgenschaefer";
    repo   = "circe";
    rev    = "v${self.version}";
    sha256 = "08dsv1dzgb9jx076ia7xbpyjpaxn1w87h6rzlb349spaydq7ih24";
  };

  fileSpecs = [ "lisp/shorten*.el" ];
})
