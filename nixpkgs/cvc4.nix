{ stdenv, fetchurl, pkgconfig, boost, gmp, curl, which
}:

stdenv.mkDerivation rec {
  name = "cvc4-1.4";

  src = fetchurl {
    url    = "http://cvc4.cs.nyu.edu/builds/src/${name}.tar.gz";
    sha256 = "093h7zgv4z4ad503j30dpn8k2pz9m90pvd7gi5axdmwsxgwlzzkn";
  };

  preConfigure = ''
    ./contrib/get-antlr-3.4

    configureFlagsArray=(
      --with-antlr-dir=$(pwd)/antlr-3.4
      ANTLR=$(pwd)/antlr-3.4/bin/antlr3
      --enable-language-bindings=c
    )
  '';

  buildInputs =
    [ pkgconfig boost gmp curl which ];

  doCheck = false;

  meta = with stdenv.lib; {
    description = "CVC4: the smt solver";
    homepage    = http://cvc4.cs.nyu.edu/web/;
    license     = licenses.bsd3;
    maintainers = with maintainers; [ ];
    platforms   = platforms.all;
  };
}
