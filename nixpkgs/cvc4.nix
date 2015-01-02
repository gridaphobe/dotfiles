{ stdenv, fetchgit, pkgconfig, boost, gmp, curl, which, autoconf, automake
, libtool, openjdk, antlr3, libantlr3c, perl
}:

stdenv.mkDerivation rec {
  name = "cvc4-HEAD";

  src = fetchgit {
    url    = "git://github.com/CVC4/CVC4.git";
    rev    = "5a285d5247b56b00895774c909f09c8ad1e3889c";
    sha256 = "5a2c2e72e6636986d2c2d5236924a2a4eeb6566cfd28e5f465f3e6e778b2ce4c";
  };

  # src = fetchurl {
  #   url    = "http://cvc4.cs.nyu.edu/builds/src/${name}.tar.gz";
  #   sha256 = "093h7zgv4z4ad503j30dpn8k2pz9m90pvd7gi5axdmwsxgwlzzkn";
  # };

  patches = [ ./fix-clang-35.patch ];

  preConfigure = ''
    ./autogen.sh

    configureFlagsArray=(
      --with-antlr-dir=${libantlr3c}
      ANTLR=${antlr3}/bin/antlr
      --enable-language-bindings=c
      CFLAGS=-O0
      CXXFLAGS=-O0
    )
  '';


  buildInputs =
    [ pkgconfig boost gmp curl which autoconf automake libtool openjdk
      antlr3 libantlr3c perl
    ];

  #doCheck = true;

  meta = with stdenv.lib; {
    description = "CVC4: the smt solver";
    homepage    = http://cvc4.cs.nyu.edu/web/;
    license     = licenses.bsd3;
    maintainers = with maintainers; [ gridaphobe ];
    platforms   = platforms.all;
  };
}
