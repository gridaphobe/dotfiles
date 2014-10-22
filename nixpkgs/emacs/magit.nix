{ melpa, fetchgit, git-commit-mode, git-rebase-mode }:

melpa.mkDerivation (self: {
  pname = "magit";
  version = "20141016";
  src = fetchgit {
    url = "git://github.com/magit/magit.git";
    rev = "fb3bfe4f9dddd628d5c4bbd63bd2de18fec906fe";
    sha256 = "623a6b45b2ea302f24aad03c76ed58588ebaead8b54ef3923e9a9b9c0f75ae7f";
  };

  packageRequires = [ git-commit-mode git-rebase-mode ];

})
