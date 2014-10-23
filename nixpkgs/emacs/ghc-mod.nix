{ melpa, ghcMod }:

melpa.mkDerivation (self: {
  pname = "ghc";
  version = ghcMod.version;
  src = ghcMod.src;
  
  fileSpecs = [ "elisp/*.el" ];

})
