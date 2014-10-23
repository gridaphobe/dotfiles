{ melpa, fetchFromGitHub }:

melpa.mkDerivation (self: {
  pname = "gitattributes-mode";
  version = "0.15.0";

  src = fetchFromGitHub {
    owner  = "magit";
    repo   = "git-modes";
    rev    = self.version;
    sha256 = "1x03276yq63cddc89n8i47k1f6p26b7a5la4hz66fdf15gmr8496";
  };
  
  files = [ "gitattributes-mode.el" ];

})
