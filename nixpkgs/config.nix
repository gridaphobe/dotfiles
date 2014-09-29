{
  allowBroken = true;
  allowUnfree = true;

  packageOverrides = pkgs: with pkgs; rec {

    # devToolsEnv = pkgs.buildEnv {
    #   name = "dev-tools";
    #   paths = [
    #     aspell
    #     aspellDicts.en
    #     # coreutils
    #     curl
    #     # fish
    #     gitAndTools.gitFull
    #     gitAndTools.hub
    #     globalHsEnv
    #     # gnused
    #     # graphviz
    #     # guile
    #     # htop
    #     # imagemagick
    #     # isync
    #     # macvim
    #     # (mu.override { emacs = myemacs; })
    #     # nodejs
    #     ocaml
    #     ocamlPackages.opam
    #     # p7zip
    #     # parallel
    #     # plantuml
    #     # perl
    #     # postgresql
    #     # python
    #     rlwrap
    #     # ruby
    #     rubyLibs.terminal_notifier
    #     # rust
    #     # silver-searcher
    #     sloccount
    #     sqlite
    #     # texLiveFull
    #     tmux
    #     tree
    #     # vimNox
    #     # weechat
    #     wget
    #     z3
    #     zsh
    #   ];
    # };
    
    shellEnv = pkgs.buildEnv {
      name = "shell-env";
      paths = [
        bash
        curl
        cvc4
        fish
        gitAndTools.gitFull
        nix-prefetch-scripts
        rlwrap
        rubyLibs.terminal_notifier
        sloccount
        tmux
        wget
        zsh
        z3
      ];
    };
    
    cvc4 = callPackage ./cvc4.nix {};
    z3 = callPackage ./z3.nix {};

    haskellEnv = haskellPackages_ghc783_profiling.ghcWithPackages (self: [
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
      self.ivory
      self.ivoryBackendC
      self.ivoryModelCheck
      self.ivoryOpts
      self.languageCQuote
    ]);

    haskellProjects = { self, super, callPackage }: {
      liquidFixpoint = callPackage ../Source/liquid/fixpoint/default.nix {
       ocaml  = ocaml;
      };
      liquidhaskell  = callPackage ../Source/liquid/haskell/default.nix {};
      LiquidCheck    = callPackage ../Source/liquid/check/default.nix {};
      
      ivory = callPackage ../Source/ivory/ivory/default.nix {};
      ivoryBackendC = callPackage ../Source/ivory/ivory-backend-c/default.nix {};
      ivoryModelCheck = callPackage ../Source/ivory/ivory-model-check/default.nix {};
      ivoryOpts = callPackage ../Source/ivory/ivory-opts/default.nix {};
      languageCQuote = callPackage ./languageCQuote.nix { fetchgit = fetchgit; };

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

    haskellPackages_wrapper = hp: recurseIntoAttrs (hp.override {
        extension = this: super: haskellProjects {
          self = this;
          super = super;
          callPackage = lib.callPackageWith this;
        };
      });

    haskellPackages_ghc783 = haskellPackages_wrapper pkgs.haskellPackages_ghc783;
    haskellPackages_ghc783_profiling = haskellPackages_wrapper pkgs.haskellPackages_ghc783_profiling;

    emacsEnv = pkgs.buildEnv {
      name = "emacs-env";
      paths = [
        emacs
        
        aspell
        aspellDicts.en

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
  };
}
