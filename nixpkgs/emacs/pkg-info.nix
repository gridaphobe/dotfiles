{ melpa, fetchFromGitHub, epl }:

melpa.mkDerivation (self: {
  pname   = "pkg-info";
  version = "20140610";

  src = fetchFromGitHub {
    owner  = "lunaryorn";
    repo   = "${self.pname}.el";
    rev    = "475cdeb0b8d44f9854e506c429eeb445787014ec";
    sha256 = "0x4nz54f2shgcw3gx66d265vxwdpdirn64gzii8dpxhsi7v86n0p";
  };
  
  packageRequires = [ epl ];

})
