{ stdenv ? (import <nixpkgs> {}).stdenv
, unzip  ? (import <nixpkgs> {}).unzip
, python ? (import <nixpkgs> {}).python
, makeWrapper ? (import <nixpkgs> {}).makeWrapper
}:

stdenv.mkDerivation rec {
  name = "z3-4.3.2";
  src = /home/gridaphobe/Downloads/z3-4.3.2-x64-ubuntu-13.10.zip;
  buildInputs = [ unzip makeWrapper ];
  
  libPath = stdenv.lib.makeLibraryPath [ stdenv.gcc.gcc ];
  dontStrip = true;
  dontPatchELF = true;
  soext = if stdenv.system == "x86_64-darwin" then ".dylib" else ".so";
  installPhase = ''
    echo $libPath
    patchelf --debug --set-interpreter "$(cat $NIX_GCC/nix-support/dynamic-linker)" --set-rpath $libPath bin/z3
    mkdir -p $out/bin $out/lib/${python.libPrefix}/site-packages $out/include
    cp include/z3.h        $out/include
    cp include/z3_api.h    $out/include
    cp include/z3_v1.h     $out/include
    cp include/z3_macros.h $out/include
    cp include/z3++.h      $out/include
    cp bin/z3              $out/bin
    cp bin/libz3${soext}   $out/lib
    cp bin/libz3${soext}   $out/lib/${python.libPrefix}/site-packages
    cp bin/z3*.pyc         $out/lib/${python.libPrefix}/site-packages
    wrapProgram "$out/bin/z3" --prefix LD_LIBRARY_PATH : \
      "${stdenv.gcc.gcc}/lib64:${stdenv.gcc.gcc}/lib"
  '';
}
