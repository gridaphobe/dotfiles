# generic builder for Emacs packages

{ stdenv, fetchurl, emacs, texinfo
, extension ? (self : super : {})
}:

let
  enableFeature         = stdenv.lib.enableFeature;
  versionOlder          = stdenv.lib.versionOlder;
  optional              = stdenv.lib.optional;
  optionals             = stdenv.lib.optionals;
  optionalString        = stdenv.lib.optionalString;
  filter                = stdenv.lib.filter;

in

{
  mkDerivation =
    args : # arguments for the individual package, can modify the defaults
    let # These attributes are removed in the end. This is in order not to spoil the build
        # environment overly, but also to keep hash-backwards-compatible with the old cabal.nix.
        internalAttrs = [
          "files" "fileSpecs" "packageRequires" "targets"
        ];

        # Stuff happening after the user preferences have been processed. We remove
        # internal attributes and strip null elements from the dependency lists, all
        # in the interest of keeping hashes stable.
        postprocess =
          x : (removeAttrs x internalAttrs) // {
                buildInputs           = filter (y : ! (y == null)) x.buildInputs;
                propagatedBuildInputs = filter (y : ! (y == null)) x.propagatedBuildInputs;
                propagatedUserEnvPkgs = filter (y : ! (y == null)) x.propagatedUserEnvPkgs;
                doCheck               = x.doCheck;
              };

        defaults =
          self : { # self is the final version of the attribute set

            # pname should be defined by the client to be the package basename
            # version should be defined by the client to be the package version

            # fname is the internal full name of the package
            fname = "${self.pname}-${self.version}";

            # name is the external full name of the package; usually we prefix
            # all packages with haskell- to avoid name clashes for libraries;
            # if that is not desired (for applications), name can be set to
            # fname.
            name = "emacs-${self.pname}-${self.version}";

            # the default download location for Cabal packages is Hackage,
            # you still have to specify the checksum
            # src = fetchurl {
            #   url = "mirror://hackage/${self.pname}/${self.fname}.tar.gz";
            #   inherit (self) sha256;
            # };

            # default buildInputs are just ghc, if more buildInputs are required
            # buildInputs can be extended by the client by using extraBuildInputs,
            # but often propagatedBuildInputs is preferable anyway
            buildInputs = [emacs texinfo] ++ self.packageRequires;

            # we make sure that propagatedBuildInputs is defined, so that we don't
            # have to check for its existence
            propagatedBuildInputs = self.packageRequires;

            # By default, also propagate all dependencies to the user environment. This is required, otherwise packages would be broken, because
            # GHC also needs all dependencies to be available.
            propagatedUserEnvPkgs = self.packageRequires;
            
            packageRequires = [];

            doCheck = false;
            
            files = [];

            fileSpecs = [ "*.el" "*.el.in" "dir"
                          "*.info" "*.texi" "*.texinfo"
                          "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                        ];
    # "*.el" "*.el.in" "dir"
    # "*.info" "*.texi" "*.texinfo"
    # "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
    # (:exclude ".dir-locals.el" "tests.el" "*-test.el" "*-tests.el")

            # compiles Setup and configures
            configurePhase = ''
              eval "$preConfigure"
              
              echo "EMACSLOADPATH=$EMACSLOADPATH"

              eval "$postConfigure"
            '';

            # builds via Cabal
            
            setupHook = ./melpa-setup.sh;
            
            targets = stdenv.lib.concatStringsSep " " 
                        (if self.files == []
                         then self.fileSpecs
                         else self.files);

            buildPhase = ''
              eval "$preBuild"
              
              emacs --batch -Q -l ${./package-build.el} -l ${./melpa2nix.el} \
                -f melpa2nix-build-package \
                $out/share/emacs/site-lisp/elpa \
                ${self.pname} ${self.version} ${self.targets}

              # mkdir ${self.fname}
              
              # cp ${toString self.targets} ${self.fname}

              # tar -cv --mtime=./${self.pname} -f ${self.fname}.tar ${self.pname}

              eval "$postBuild"
            '';

            checkPhase = optional self.doCheck ''
              eval "$preCheck"


              eval "$postCheck"
            '';

            # installs via Cabal; creates a registration file for nix-support
            # so that the package can be used in other Haskell-builds; also
            # adds all propagated build inputs to the user environment packages
            installPhase = ''
              eval "$preInstall"

              # emacs --batch -q -l ${./melpa2nix.el} --eval \
              #   "(progn (setq package-user-dir \"$out/share/emacs/site-lisp/elpa\") \
              #           (package-initialize) \
              #           (package-install-file \"${self.fname}.tar\")
              #    )"

              if test -f $out/nix-support/propagated-native-build-inputs; then
                ln -s $out/nix-support/propagated-native-build-inputs $out/nix-support/propagated-user-env-packages
              fi

              eval "$postInstall"
            '';

            # We inherit stdenv and emacs so that they can be used
            # in melpa derivations.
            inherit stdenv emacs texinfo;
          };
    in
    stdenv.mkDerivation (postprocess (let super = defaults self // args self;
                                          self  = super // extension self super;
                                      in self));
}
