{ melpa, fetchFromGitHub, evil }:

melpa.mkDerivation (self: {
  pname   = "evil-surround";
  version = "20140616";

  src = fetchFromGitHub {
    owner  = "timcharper";
    repo   = self.pname;
    rev    = "71f380b6b6ed38f739c0a4740b3d6de0c52f915a";
    sha256 = "0wrmlmgr4mwxlmmh8blplddri2lpk4g8k3l1vpb5c6a975420qvn";
  };
  
  packageRequires = [ evil ];

})
