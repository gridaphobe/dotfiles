{ stdenv, fetchgit, emacs, texinfo, gitModes }:

stdenv.mkDerivation {
  name = "magit-6556c94";
  src = fetchgit {
    url = "git://github.com/magit/magit.git";
    rev = "6556c94aafb13d48a4bd54ba0f8056c81496dba0";
    sha256 = "f57b714e15f0e0396ccfab9a4d6c37081267ba96fccf5c042f82ca31d09b5c3c";
  };
  preConfigure = ''
    makeFlagsArray=( 
      PREFIX=$out 
      SYSCONFDIR=$out/etc 
      EFLAGS="-L ${gitModes}/share/emacs/site-lisp"
    )
  '';        
  buildInputs = [ emacs texinfo gitModes ];
}
