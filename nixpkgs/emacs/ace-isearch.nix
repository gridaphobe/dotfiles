{ melpa, fetchFromGitHub, ace-jump-mode, helm-swoop }:

melpa.mkDerivation (self: {
  pname   = "ace-isearch";
  version = "20150105";

  src = fetchFromGitHub {
    owner  = "tam17aki";
    repo   = self.pname;
    rev    = "be083913a2bd87451421e8940344576475406108";
    sha256 = "08brrwp2s9drdagbdxa9b9wd60l28czf3ds17bfn4i7npy4mz5fs";
  };
  
  packageRequires = [ ace-jump-mode helm-swoop ];

})
