{ melpa, fetchgit }:

melpa.mkDerivation (self: {
  pname   = "gnus";
  version = "20140501";

  src = fetchgit {
    url = "http://git.gnus.org/gnus.git";
    rev = "4228cffcb7afb77cf39678e4a8988a57753502a5";
    sha256 = "0qd0wpxkz47irxghmdpa524c9626164p8vgqs26wlpbdwyvm64a0";
  };
  
  fileSpecs = [ "lisp/*.el" "texi/*.texi" ];
  
  preBuild = ''
    ./configure
    make
  '';
})
