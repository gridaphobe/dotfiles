{ melpa, fetchgit }:

melpa.mkDerivation (self: {
  pname   = "goto-chg";
  version = "1.6";

  src = fetchgit {
    url = "git://gitorious.org/evil/evil";
    rev = "999ec15587f85100311c031aa8efb5d50c35afe4";
    sha256 = "18736fc8100801ab1198b33fbf0f08cf4a3779dd80771dfa3f700e7182ac9f6d";
  };
  
  files = [ "lib/goto-chg.el" ];

})
