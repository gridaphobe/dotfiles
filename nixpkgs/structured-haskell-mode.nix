{ cabal, haskellSrcExts, fetchFromGitHub }:

cabal.mkDerivation (self: {
  pname = "structured-haskell-mode";
  version = "20141129";
  src = fetchFromGitHub {
    owner = "chrisdone";
    repo = self.pname;
    rev  = "92f0accef811e091eeb2d72f1fc567f1a39285a4";
    sha256 = "1kif8f97knx9klfg39674597sjlcd89jn30vbv9dvdz0cjkfqp8b";
  };
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
