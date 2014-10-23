{ melpa, fetchgit }:

melpa.mkDerivation (self: {
  pname   = "undo-tree";
  version = "0.6.4";

  src = fetchgit {
    url    = "http://www.dr-qubit.org/git/${self.pname}.git";
    rev    = "a3e81b682053a81e082139300ef0a913a7a610a2";
    sha256 = "1qla7njkb7gx5aj87i8x6ni8jfk1k78ivwfiiws3gpbnyiydpx8y";
  };
  
  # unpackPhase = ''
  #   mkdir undo-tree
  #   cp $src undo-tree/undo-tree.el
  #   cd undo-tree
  # '';

})
