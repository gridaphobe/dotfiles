# This file was auto-generated by cabal2nix. Please do NOT edit manually!

{ cabal, attoparsec, time }:

cabal.mkDerivation (self: {
  pname = "toml";
  version = "0.1.3";
  sha256 = "0wby1jas854niwyac95n39liqc874xcd1ahqpw6ksi2nhv2ld6f2";
  buildDepends = [ attoparsec time ];
  meta = {
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})