{ melpa, ghcMod }:

melpa.mkDerivation (self: {
  pname   = "ghc-mod";
  version = ghcMod.version;
  src = ghcMod.src;

  preConfigure = ''
    cd elisp
  '';
  
})
