{ melpa, fetchFromGitHub, async }:

melpa.mkDerivation (self: {
  pname   = "helm";
  version = "20141022";

  src = fetchFromGitHub {
    owner  = "emacs-helm";
    repo   = self.pname;
    rev    = "c0acb0f7dc5360fea754f06995669a0cd369c100";
    sha256 = "0rmq08xqyy5qw33cdhv43vjqzkqajhms1cx798iknb61l8nslayw";
  };
  
  packageRequires = [ async ];

})
