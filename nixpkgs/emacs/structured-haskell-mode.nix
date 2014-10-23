{ melpa, haskell-mode, structuredHaskellMode }:

melpa.mkDerivation (self: {
  pname = "shm";
  version = structuredHaskellMode.version;
  src = structuredHaskellMode.src;
  
  packageRequires = [ haskell-mode ];
  fileSpecs = [ "elisp/*.el" ];

  meta = {
    homepage = "https://github.com/chrisdone/structured-haskell-mode";
    description = "Structured editing Emacs mode for Haskell";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
