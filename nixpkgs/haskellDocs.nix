# This file was auto-generated by cabal2nix. Please do NOT edit manually!

{ cabal, aeson, Cabal, filepath, ghcPaths, haddock, hscolour, monadLoops
, text, unorderedContainers
}:

cabal.mkDerivation (self: {
  pname = "haskell-docs";
  version = "4.2.2";
  sha256 = "0w52kzwjzd5jl8v55rjy5550cy2fcyj9j5b7b33vbwjyh06gfrk1";
  isLibrary = true;
  isExecutable = true;
  extraBuildInputs = [ haddock hscolour ];
  buildDepends = [
    aeson Cabal filepath ghcPaths monadLoops text
    unorderedContainers
  ];
  meta = {
    homepage = "http://github.com/chrisdone/haskell-docs";
    description = "A program to find and display the docs and type of a name";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})