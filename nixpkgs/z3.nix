{ stdenv, fetchurl, python, unzip }:

stdenv.mkDerivation rec {
  name = "z3-${version}";
  version = "4.3.2";
  src = fetchurl {
    url    = "http://download-codeplex.sec.s-msft.com/Download/Release?ProjectName=z3&DownloadId=882592&FileTime=130507819772300000&Build=20919";
    name   = "${name}.zip";
    sha256 = "1y6fzc3hvqj9hbj5jm14zy84y3p2z1drv3fzppw1yixcqifg7w48";
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
