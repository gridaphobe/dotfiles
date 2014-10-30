{ melpa, fetchgit, goto-chg, undo-tree }:

melpa.mkDerivation (self: {
  pname   = "evil";
  version = "20141020";

  src = fetchgit {
    url = "git://gitorious.org/evil/evil";
    rev = "999ec15587f85100311c031aa8efb5d50c35afe4";
    sha256 = "0yiqpzsm5sr7xdkixdvfg312dk9vsdcmj69gizk744d334yn8rsz";
  };
  
  packageRequires = [ goto-chg undo-tree ];

})
