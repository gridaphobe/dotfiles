{ melpa, fetchFromGitHub, flycheck, popup-el }:

melpa.mkDerivation (self: {
  pname   = "flycheck-pos-tip";
  version = "20140813";

  src = fetchFromGitHub {
    owner  = "flycheck";
    repo   = self.pname;
    rev    = "5b3a203bbdb03e4f48d1654efecd71f44376e199";
    sha256 = "0b4x24aq0jh4j4bjv0fqyaz6hzh3gqf57k9763jj9rl32cc3dpnp";
  };
  
  packageRequires = [ flycheck popup-el ];

})
