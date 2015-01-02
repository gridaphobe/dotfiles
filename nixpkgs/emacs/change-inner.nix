{ melpa, fetchFromGitHub, expand-region }:

melpa.mkDerivation (self: {
  pname   = "change-inner";
  version = "20130208";

  src = fetchFromGitHub {
    owner  = "magnars";
    repo   = "${self.pname}.el";
    rev    = "6374b745ee1fd0302ad8596cdb7aca1bef33a730";
    sha256 = "1fv8630bqbmfr56zai08f1q4dywksmghhm70084bz4vbs6rzdsbq";
  };

  packageRequires = [ expand-region ];

})
