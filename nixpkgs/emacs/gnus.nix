{ melpa, fetchurl }:

melpa.mkDerivation (self: {
  pname   = "gnus";
  version = "20140501";

  src = fetchurl {
    url = "http://git.gnus.org/cgit/gnus.git/snapshot/m0-11.zip";
    sha256 = "1c05x9vzaw34s88v015h5yills0g23af4h818c8gv7myvg8jxh8v";
  };
  
  fileSpecs = [ "lisp/*.el" "texi/*.texi" ];
  
  preBuild = ''
    ./configure
    make
  '';
})
