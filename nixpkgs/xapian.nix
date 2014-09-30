{ stdenv, fetchurl, libuuid, zlib }:

stdenv.mkDerivation {
  name = "xapian-1.2.18";

  src = fetchurl {
    url = http://oligarchy.co.uk/xapian/1.2.18/xapian-core-1.2.18.tar.xz;
    sha256 = "16i063xzwxdrqy32vlr292lljb65hkg3xx0i2m0qx2v00pcn4b3n";
  };

  buildInputs = [ libuuid zlib ];

  meta = { 
    description = "Search engine library";
    homepage = "http://xapian.org";
    license = stdenv.lib.licenses.gpl2Plus;
    maintainers = [ stdenv.lib.maintainers.chaoflow ];
  };
}
