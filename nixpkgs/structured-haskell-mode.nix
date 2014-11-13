{ cabal, haskellSrcExts }:

cabal.mkDerivation (self: {
  pname = "structured-haskell-mode";
  version = "1.0.4";
  sha256 = "1402wx27py7292ad7whsb13ywv71k36501jpfrn2p0v7knzknj8z";
  isLibrary = false;
  isExecutable = true;
  buildDepends = [ haskellSrcExts ];
  meta = {
    homepage = "https://github.com/chrisdone/structured-haskell-mode";
    description = "Structured editing Emacs mode for Haskell";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
