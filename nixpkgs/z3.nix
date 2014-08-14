{ stdenv, fetchurl, python, unzip }:

stdenv.mkDerivation rec {
  name = "z3-${version}";
  version = "4.3.2";
  src = fetchurl {
    url    = "http://s3.amazonaws.com/gridaphobe/z3-4.3.2.5a45711f22d9-x64-osx-10.9.2.xz";
    name   = "${name}.tar.xz";
    sha256 = "14q9qjh5iip3zk7v062wvhp93b57zrs5z63pk3wrka0b8dmc56g1";
  };

  buildInputs = [ python unzip ];

  # The zip file doesn't unpack a directory, just the code itself.
  # unpackPhase = "mkdir ${name} && cd ${name} && unzip $src";
  # postConfigure = ''
  #   python scripts/mk_make.py
  #   cd build
  # '';

  # z3's install phase is stupid because it tries to calculate the
  # python package store location itself, meaning it'll attempt to
  # write files into the nix store, and fail.
  soext = if stdenv.system == "x86_64-darwin" then ".dylib" else ".so";
  installPhase = ''
    mkdir -p $out/bin $out/lib/${python.libPrefix}/site-packages $out/include
    cp include/z3.h        $out/include
    cp include/z3_api.h    $out/include
    cp include/z3_v1.h     $out/include
    cp include/z3_macros.h $out/include
    cp include/z3++.h  $out/include
    cp bin/z3                     $out/bin
    cp bin/libz3${soext}          $out/lib
    cp bin/libz3${soext}          $out/lib/${python.libPrefix}/site-packages
    cp bin/z3*.pyc                $out/lib/${python.libPrefix}/site-packages
  '';

  meta = {
    description = "Z3 is a high-performance theorem prover and SMT solver";
    homepage    = "http://z3.codeplex.com";
    license     = stdenv.lib.licenses.msrla;
    platforms   = stdenv.lib.platforms.unix;
    maintainers = [ stdenv.lib.maintainers.thoughtpolice ];
  };
}
