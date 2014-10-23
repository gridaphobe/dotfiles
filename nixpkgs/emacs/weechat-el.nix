{ stdenv, melpa, fetchFromGitHub, s-el }:

melpa.mkDerivation (self: {
  pname   = "weechat.el";
  version = "20141016";

  src = fetchFromGitHub {
    owner  = "the-kenny";
    repo   = self.pname;
    rev    = "4cb2ced1eda5167ce774e04657d2cd077b63c706";
    sha256 = "003sihp7irm0qqba778dx0gf8xhkxd1xk7ig5kgkryvl2jyirk28";
  };
  
  preConfigure = stdenv.lib.optionalString (!stdenv.isLinux) 
    "rm weechat-sauron.el weechat-secrets.el";
  
  packageRequires = [ s-el ];

})
