{
  allowBroken = true;
  allowUnfree = true;

  packageOverrides = pkgs: with pkgs; rec {

    devToolsEnv = pkgs.buildEnv {
      name = "dev-tools";
      paths = [
        aspell
        aspellDicts.en
        coreutils
        curl
        emacs
        emacs24Packages.structuredHaskellMode
        fish
        gitAndTools.gitFull
        gitAndTools.hub
        globalHsEnv
        gnused
        graphviz
        # guile
        htop
        # imagemagick
        isync
        # macvim
        mu
        #(mu.override { emacs = myemacs; })
        nodejs
        ocaml
        ocamlPackages.opam
        p7zip
        parallel
        # plantuml
        # perl
        postgresql
        python
        rlwrap
        ruby
        rubyLibs.terminal_notifier
        # rust
        silver-searcher
        sloccount
        sqlite
        # texLiveFull
        tmux
        tree
        # vimNox
        # weechat
        wget
        z3
        zsh
      ];
    };

    # z3 = callPackage ./z3.nix {};

    globalHsEnv = haskellPackages_ghc783_profiling.ghcWithPackages (self: [
      self.cabal2nix
      self.cabalInstall
      self.ghcCore
      self.ghcMod
      #self.haskellDocs
      self.hasktags
      #self.hdevtools
      self.hlint
      self.hscolour
      # self.myHoogleLocal
      self.stylishHaskell
      self.pandoc
      self.pandocCiteproc
      self.pandocTypes
      
      self.text

      self.QuickCheck
      self.smallcheck
      self.criterion
      self.tasty
      self.tastyHunit
      self.tastyRerun      

      #self.liquidFixpoint
      #self.liquidhaskell
      #self.LiquidCheck
    ]);

    haskellProjects = { self, super, callPackage }: {
      #liquidFixpoint = callPackage /Users/gridaphobe/Source/liquid/fixpoint/cabal.nix {
      #  ocaml  = ocaml;
      #  z3     = z3;
      #};
      #liquidhaskell  = callPackage /Users/gridaphobe/Source/liquid/haskell/cabal.nix {};
      #LiquidCheck    = callPackage /Users/gridaphobe/Source/liquid/check/cabal.nix {};
      #hdevtools      = callPackage /Users/gridaphobe/Source/hdevtools {};
      haskellDocs    = self.disableTest (callPackage ./haskellDocs.nix {});
      systemFileio   = self.disableTest  super.systemFileio;
      shake          = self.disableTest  super.shake;
      intern         = callPackage ./intern.nix {};
      dataTextual    = callPackage ./dataTextual.nix {};
      dataTimeout    = callPackage ./dataTimeout.nix {};
      textLatin1     = callPackage ./textLatin1.nix {};
      textPrinter    = callPackage ./textPrinter.nix {};
      typeHint       = callPackage ./typeHint.nix {};

      #myHoogleLocal  = self.hoogleLocal.override {
      #  packages = with self; ([
      #    ghc
      #    liquidFixpoint
      #    liquidhaskell
      #    LiquidCheck
      #  ] ++ liquidFixpoint.propagatedNativeBuildInputs
      #    ++ liquidhaskell.propagatedNativeBuildInputs
      #    ++ LiquidCheck.propagatedNativeBuildInputs
      #  );
      #};
    };

    myemacs = callPackage ./emacs.nix {};
    emacs   = myemacs;
    emacs24Packages = recurseIntoAttrs (emacsPackages myemacs pkgs.emacs24Packages);
    # emacs = lib.overrideDerivation pkgs.emacs (oldAttrs: rec {
    #   name = "emacs-24.3.92";
    #   src = fetchurl {
    #     url    = "http://alpha.gnu.org/gnu/emacs/pretest/${name}.tar.xz";
    #     sha256 = "1dxy6hxpj40ahpq3qrndpfra8d0q2wn05qb50dah08g2rfbm1bp5";
    #   };
    #   configureFlags = [ "--with-gnutls" "--with-imagemagick" "--with-rsvg" "--with-ns" ];
    #   buildInputs = oldAttrs.buildInputs ++ [ autoconf automake ];
    #   builder = stdenv.builder;
    #   preConfigure = ''
    #     ./autogen.sh
    #   '';
    # });
    #emacs = pkgs.emacs24Macport;
    #emacs24Packages = recurseIntoAttrs (emacsPackages emacs24Macport pkgs.emacs24Packages);

    haskellPackages_wrapper = hp: recurseIntoAttrs (hp.override {
        extension = this: super: haskellProjects {
          self = this;
          super = super;
          callPackage = lib.callPackageWith this;
        };
      });

    haskellPackages_ghc783 = haskellPackages_wrapper pkgs.haskellPackages_ghc783;
    haskellPackages_ghc783_profiling = haskellPackages_wrapper pkgs.haskellPackages_ghc783_profiling;


    # hsEnv = { name, ghc, deps }:
    #   let hsPkgs = ghc.ghcWithPackages (self : ([
    #       # self.cabal2nix
    #       self.cabalInstall
    #       #self.ghcCore
    #       #self.ghcMod
    #       #self.hasktags
    #       #self.HaRe
    #       #self.hdevtools
    #       self.hlint
    #     ] ++ (deps self)));
    #   in
    #     pkgs.myEnvFun {
    #       name = name;
    #       buildInputs = [
    #         pkgs.binutils
    #         pkgs.coreutils
    #         pkgs.zsh
    #         hsPkgs
    #       ];
    #       shell = "${pkgs.zsh.outPath}/bin/zsh";
    #       extraCmds = ''
    #         $(grep export ${hsPkgs.outPath}/bin/ghc)
    #       '';
    #     };
  };
}
