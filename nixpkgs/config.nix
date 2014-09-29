{
  allowBroken = true;
  allowUnfree = true;

  packageOverrides = pkgs: with pkgs; rec {

    devToolsEnv = pkgs.buildEnv {
      name = "dev-tools";
      paths = [
        aspell
        aspellDicts.en
        # coreutils
        curl
        # fish
        gitAndTools.gitFull
        gitAndTools.hub
        globalHsEnv
        # gnused
        # graphviz
        # guile
        # htop
        # imagemagick
        # isync
        # macvim
        # (mu.override { emacs = myemacs; })
        # nodejs
        ocaml
        ocamlPackages.opam
        # p7zip
        # parallel
        # plantuml
        # perl
        # postgresql
        # python
        rlwrap
        # ruby
        rubyLibs.terminal_notifier
        # rust
        # silver-searcher
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
    
    z3 = callPackage ./z3.nix {};

    globalHsEnv = haskellPackages_ghc783_profiling.ghcWithPackages (self: [
      self.cabal2nix
      self.cabalInstall
      self.ghcCore
      self.ghcMod
      # self.haskellDocs
      self.hasktags
      # self.hdevtools
      self.hlint
      self.hscolour
      self.hoogleLocal
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

      self.liquidFixpoint
      self.liquidhaskell
      self.LiquidCheck
    ]);

    haskellProjects = { self, super, callPackage }: {
      liquidFixpoint = callPackage /Users/gridaphobe/Source/liquid/fixpoint/cabal.nix {
        ocaml  = ocaml;
      };
      liquidhaskell  = callPackage /Users/gridaphobe/Source/liquid/haskell/cabal.nix {};
      LiquidCheck    = callPackage /Users/gridaphobe/Source/liquid/check/cabal.nix {};
      haskellDocs    = self.disableTest (callPackage ./haskellDocs.nix {});
      systemFileio   = self.disableTest  super.systemFileio;
      shake          = self.disableTest  super.shake;
      intern         = callPackage ./intern.nix {};
      dataTextual    = callPackage ./dataTextual.nix {};
      dataTimeout    = callPackage ./dataTimeout.nix {};
      textLatin1     = callPackage ./textLatin1.nix {};
      textPrinter    = callPackage ./textPrinter.nix {};
      typeHint       = callPackage ./typeHint.nix {};
      
      monadJournal   = callPackage ./monad-journal.nix {};
      ghcMod         = self.disableTest (callPackage ./ghc-mod.nix { emacs = emacs; });

      hoogleLocal    = super.hoogleLocal.override {
        packages = with self; ([
          ghc
          liquidFixpoint
          liquidhaskell
          LiquidCheck
        ] ++ liquidFixpoint.propagatedNativeBuildInputs
          ++ liquidhaskell.propagatedNativeBuildInputs
          ++ LiquidCheck.propagatedNativeBuildInputs
        );
      };
    };

    emacsEnv = pkgs.buildEnv {
      name = "emacs-env";
      paths = [
        emacs

        companyMode
        dash
        epl
        evil
        exec-path-from-shell
        gitModes
        haskellPackages_ghc783_profiling.ghcMod
        haskellMode
        helm
        magit
        pkg-info
        projectile
        richMinority
        s-el
        smartModeLine
        smartparens
        structuredHaskellMode
        undoTree
        usePackage
        # emacs24Packages.org
      ];
    };

    myemacs = callPackage ./emacs.nix {};
    emacs   = myemacs;
    emacs24Packages = recurseIntoAttrs (emacsPackages myemacs pkgs.emacs24Packages);
    
    companyMode = callPackage ./emacs/company-mode.nix {};
    dash = callPackage ./emacs/dash.nix {};
    evil = callPackage ./emacs/evil.nix {};
    epl = callPackage ./emacs/epl.nix {};
    exec-path-from-shell = callPackage ./emacs/exec-path-from-shell.nix {};
    gitModes = callPackage ./emacs/git-modes.nix {};
    helm = callPackage ./emacs/helm.nix {};
    magit = callPackage ./emacs/magit.nix {};
    richMinority = callPackage ./emacs/rich-minority.nix {};
    projectile = callPackage ./emacs/projectile.nix {};
    pkg-info = callPackage ./emacs/pkg-info.nix {};
    s-el = callPackage ./emacs/s-el.nix {};
    smartModeLine = callPackage ./emacs/smart-mode-line.nix {};
    smartparens = callPackage ./emacs/smartparens.nix {};
    undoTree = callPackage ./emacs/undo-tree.nix {};
    usePackage = callPackage ./emacs/use-package.nix {};

    haskellMode = callPackage ./emacs/haskellMode.nix {};
    # haskellMode = lib.overrideDerivation emacs24Packages.haskellMode (attrs: {
    #   name = "haskell-mode-3ca8c10";
    #   src = fetchgit {
    #     url = "git://github.com/haskell/haskell-mode.git";
    #     rev = "3ca8c1097a3a270cd7dcf77d7109b47550337979";
    #     sha256 = "2debd11403719df103f196face4eb9d8934a97f5e086e8f51d1f88b95bd232cf";
    #   };
    # });
    structuredHaskellMode = lib.overrideDerivation 
                            emacs24Packages.structuredHaskellMode (attrs: {
      name = "structured-haskell-mode-d025da6";
      src = fetchgit {
        url = "git://github.com/chrisdone/structured-haskell-mode.git";
        rev = "d025da601c80669b8618d09836dd2a54f5fb9c1a";
        sha256 = "f1b26d89d953019d269ba1badeed6ded08d786abb623bf5f3fb1d281b7b655bc";
      };
    });
    # ghcMod-el = callPackage ./emacs/ghc-mod.nix { ghcMod = haskellPackages_ghc783_profiling.ghcMod; };

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
