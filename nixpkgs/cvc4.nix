{ stdenv, fetchgit, pkgconfig, boost, gmp, curl, which, autoconf, automake
, libtool, openjdk, antlr3, libantlr3c
}:

stdenv.mkDerivation rec {
  name = "cvc4-HEAD";

  src = fetchgit {
    url    = "git://github.com/CVC4/CVC4.git";
    rev    = "28027d15202a0dea0c13f5b01188ec3f1c4f0c38";
    sha256 = "2546e490799727fa2125f43cfc30c8fb66f298393b82ae1d0f4b2af5e51d5e2d";
  };

  # src = fetchurl {
  #   url    = "http://cvc4.cs.nyu.edu/builds/src/${name}.tar.gz";
  #   sha256 = "093h7zgv4z4ad503j30dpn8k2pz9m90pvd7gi5axdmwsxgwlzzkn";
  # };

  preConfigure = ''
    ./autogen.sh

    configureFlagsArray=(
      --with-antlr-dir=${libantlr3c}
      ANTLR=${antlr3}/bin/antlr
      --enable-language-bindings=c
    )
  '';

  buildInputs =
    [ pkgconfig boost gmp curl which autoconf automake libtool openjdk
      antlr3 libantlr3c
    ];

  doCheck = true;

  meta = with stdenv.lib; {
    description = "CVC4: the smt solver";
    homepage    = http://cvc4.cs.nyu.edu/web/;
    license     = licenses.bsd3;
    maintainers = with maintainers; [ ];
    platforms   = platforms.all;
  };
}
