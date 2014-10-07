{ stdenv, fetchgit, pkgconfig, boost, gmp, curl, which, autoconf, automake, libtool, openjdk
}:

stdenv.mkDerivation rec {
  name = "cvc4-HEAD";

  src = fetchgit {
    url    = "git://github.com/CVC4/CVC4.git";
    rev    = "ff788863577dbc8d15a584d869f543773ce0ff1d";
    sha256 = "a1cefb938c2bfc6899819d5beadc5e98b21ef46f28e163f3d999717d7d7477b1";
  };

  # src = fetchurl {
  #   url    = "http://cvc4.cs.nyu.edu/builds/src/${name}.tar.gz";
  #   sha256 = "093h7zgv4z4ad503j30dpn8k2pz9m90pvd7gi5axdmwsxgwlzzkn";
  # };

  preConfigure = ''
    ./autogen.sh
    ./contrib/get-antlr-3.4

    configureFlagsArray=(
      --with-antlr-dir=$(pwd)/antlr-3.4
      ANTLR=$(pwd)/antlr-3.4/bin/antlr3
      --enable-language-bindings=c
    )
  '';

  buildInputs =
    [ pkgconfig boost gmp curl which autoconf automake libtool openjdk ];

  doCheck = false;

  meta = with stdenv.lib; {
    description = "CVC4: the smt solver";
    homepage    = http://cvc4.cs.nyu.edu/web/;
    license     = licenses.bsd3;
    maintainers = with maintainers; [ ];
    platforms   = platforms.all;
  };
}
