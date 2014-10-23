{ melpa, fetchFromGitHub, dash }:

melpa.mkDerivation (self: {
  pname   = "rich-minority";
  version = "0.1.1";

  src = fetchFromGitHub {
    owner  = "Bruce-Connor";
    repo   = self.pname;
    rev    = self.version;
    sha256 = "0kvhy4mgs9llihwsb1a9n5a85xzjiyiyawxnz0axy2bvwcxnp20k";
  };
  
  packageRequires = [ dash ];

})
