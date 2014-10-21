{
  allowBroken = true;
  allowUnfree = true;

  packageOverrides = pkgs: with pkgs; rec {

    makeShell = p: extras: lib.overrideDerivation p (attrs: {
      buildInputs = [fish gitAndTools.git] ++ extras ++ attrs.buildInputs;
      shellHook = "exec fish";
    });

    shellEnv = pkgs.buildEnv {
      name = "shell-env";
      paths = [
        # acl2
        arcanist
        autoconf
        automake
        bashInteractive
        cacert
        coreutils
        curl
        # cvc4
        findutils
        fish
        gitAndTools.git
        gitAndTools.hub
        gnugrep
        gnumake
        gnupatch
        gnupg
        gnused
        gnutar
        isync
        # mu
        nix-prefetch-scripts
        # notmuch
        pkgconfig
        rlwrap
        rubyLibs.terminal_notifier
        sloccount
        tmux
        tree
        wget
        zsh
        z3
      ];
    };

    # bash = pkgs.bashInteractive;

    cvc4 = callPackage ./cvc4.nix {};
    xapian = callPackage ./xapian.nix {};
    z3 = callPackage ./z3.nix {};

    haskellEnv = pkgs.buildEnv {
      name = "haskell-env";
      paths = [
        (haskellPackages_ghc783.ghcWithPackages (self: [
          self.cabal2nix
          self.cabalInstall
          self.ghcCore
          self.ghcMod
          # self.haskellDocs
          self.hasktags
          # self.hdevtools
          self.hlint
          self.hscolour
          self.stylishHaskell
          self.pandoc
          self.pandocCiteproc
          self.pandocTypes
          self.shake
          self.SafeSemaphore
          
          self.lens
          self.trifecta
          
          self.Chart
          self.ChartDiagrams
          self.text
          self.prettyShow

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
          self.ivoryBackendAcl2
          self.ivoryBackendC
          self.ivoryExamples
          self.ivoryHw
          self.ivoryModelCheck
          self.ivoryOpts
          self.ivoryQuickcheck
          self.ivorySerialize
          self.ivoryStdlib
          self.languageCQuote
          self.wlPprint
        ]))
        haskellPackages_ghc783.hoogleLocal
      ];
    };

    haskellProjects = { self, super, callPackage }: {
      liquidFixpoint = callPackage ../Source/liquid/fixpoint/default.nix {
        ocaml  = ocaml;
      };
      liquidhaskell  = callPackage ../Source/liquid/haskell/default.nix {};
      LiquidCheck    = callPackage ../Source/liquid/check/default.nix {};
      
      ivory           = callPackage ../Source/ivory/ivory/default.nix {};
      ivoryBackendAcl2   = callPackage ../Source/ivory/ivory-backend-acl2/default.nix {};
      ivoryBackendC   = callPackage ../Source/ivory/ivory-backend-c/default.nix {};
      ivoryExamples   = callPackage ../Source/ivory/ivory-examples/default.nix {};
      ivoryHw         = callPackage ../Source/ivory/ivory-hw/default.nix {};
      ivoryModelCheck = callPackage ../Source/ivory/ivory-model-check/default.nix {};
      ivoryOpts       = callPackage ../Source/ivory/ivory-opts/default.nix {};
      ivoryQuickcheck = callPackage ../Source/ivory/ivory-quickcheck/default.nix {};
      ivorySerialize  = callPackage ../Source/ivory/ivory-serialize/default.nix {};
      ivoryStdlib     = callPackage ../Source/ivory/ivory-stdlib/default.nix {};

      languageCQuote = callPackage ./languageCQuote.nix {};

      attoparsec     = self.disableTest  super.attoparsec;
      #hdevtools      = callPackage /Users/gridaphobe/Source/hdevtools {};
      haskellDocs    = self.disableTest (callPackage ./haskellDocs.nix {});
      systemFileio   = self.disableTest  super.systemFileio;
      shake          = self.disableTest  super.shake;
      lens           = self.disableTest  super.lens;
      acl2           = callPackage ../Source/acl2/default.nix {};
      intern         = callPackage ./intern.nix {};
      dataTextual    = callPackage ./dataTextual.nix {};
      dataTimeout    = callPackage ./dataTimeout.nix {};
      textLatin1     = callPackage ./textLatin1.nix {};
      textPrinter    = callPackage ./textPrinter.nix {};
      typeHint       = callPackage ./typeHint.nix {};

      Chart          = callPackage ./Chart.nix {};
      ChartDiagrams  = callPackage ./ChartDiagrams.nix {};
      
      monadJournal   = callPackage ./monad-journal.nix {};
      ghcMod         = self.disableTest (callPackage ./ghc-mod.nix { emacs = emacs; });

      hoogleLocal    = super.hoogleLocal.override {
        packages = with self; ([
          ghc
          ivory
          ivoryModelCheck
          ivoryOpts
          liquidFixpoint
          liquidhaskell
          LiquidCheck
          Chart
          ChartDiagrams
        ] ++ liquidFixpoint.propagatedNativeBuildInputs
          ++ liquidhaskell.propagatedNativeBuildInputs
          ++ LiquidCheck.propagatedNativeBuildInputs
          ++ ivory.propagatedNativeBuildInputs
          ++ Chart.propagatedNativeBuildInputs
          ++ ChartDiagrams.propagatedNativeBuildInputs
        );
      };
    };
    
    haskellFilterSource = paths: src: builtins.filterSource (path: type:
        let baseName = baseNameOf path; in
        !(builtins.elem baseName ([".git" ".cabal-sandbox" "dist"] ++ paths)))
      src;


    haskellPackages_wrapper = hp: recurseIntoAttrs (hp.override {
        extension = this: super: haskellProjects {
          self = this;
          super = super;
          callPackage = lib.callPackageWith this;
        };
      });

    haskellPackages_ghc783 = haskellPackages_wrapper pkgs.haskellPackages_ghc783;
    haskellPackages_ghc783_profiling = haskellPackages_wrapper pkgs.haskellPackages_ghc783_profiling;

    # Define own GHC HEAD package pointing to local checkout.
    packages_ghcHEAD = pkgs.haskell.packages {
      ghcPath = ../Source/ghc;
      ghcBinary = pkgs.haskellPackages.ghcPlain;
      prefFun = pkgs.haskell.ghcHEADPrefs;
    };

    haskellPackages_ghcHEAD = haskellPackages_wrapper packages_ghcHEAD;
    haskellPackages_ghcHEAD_profiling = haskellPackages_wrapper packages_ghcHEAD.profiling;


    vimEnv = pkgs.buildEnv {
      name = "vim-env";
      paths = [
        macvim
        #(vimNox.override {
        #  luaSupport    = true;
        #  pythonSupport = true;
        #  rubySupport   = true;
        #  tclSupport    = true;

        #  ftNixSupport  = true;

        #  multibyteSupport = true;
        #})

        vimPlugins.align
        vimPlugins.commentary
        vimPlugins.fugitive
        vimPlugins.ghcmod
        vimPlugins.hasksyn
        vimPlugins.hoogle
        vimPlugins.stylishHaskell
        vimPlugins.syntastic
        vimPlugins.tmuxNavigator
      ];
    };

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
        evilGodState
        exec-path-from-shell
        gitModes
        godMode
        haskellMode
        helm
        magit
        # mu4eMaildirsExtension
        pkg-info
        projectile
        richMinority
        s-el
        smartModeLine
        smartparens
        #structuredHaskellMode
        switch-window
        undoTree
        usePackage
        weechat-el

        #emacs24Packages.org
      ];
    };

    mu = pkgs.mu.override { libsoup = (libsoup.override { gnomeSupport = false; }); };

    # myemacs = callPackage ./emacs.nix {};
    myemacs = pkgs.emacs24Macport;
    emacs   = myemacs;
    emacs24Packages = recurseIntoAttrs (emacsPackages myemacs pkgs.emacs24Packages);

    companyMode = callPackage ./emacs/company-mode.nix {};
    dash = callPackage ./emacs/dash.nix {};
    evil = callPackage ./emacs/evil.nix {};
    evilGodState = callPackage ./emacs/evil-god-state.nix {};
    epl = callPackage ./emacs/epl.nix {};
    exec-path-from-shell = callPackage ./emacs/exec-path-from-shell.nix {};
    gitModes = callPackage ./emacs/git-modes.nix {};
    godMode = callPackage ./emacs/god-mode.nix {};
    helm = callPackage ./emacs/helm.nix {};
    magit = callPackage ./emacs/magit.nix {};
    mu4eMaildirsExtension = callPackage ./emacs/mu4e-maildirs-extension.nix { mu = mu; };
    richMinority = callPackage ./emacs/rich-minority.nix {};
    projectile = callPackage ./emacs/projectile.nix {};
    pkg-info = callPackage ./emacs/pkg-info.nix {};
    s-el = callPackage ./emacs/s-el.nix {};
    smartModeLine = callPackage ./emacs/smart-mode-line.nix {};
    smartparens = callPackage ./emacs/smartparens.nix {};
    switch-window = callPackage ./emacs/switch-window.nix {};
    undoTree = callPackage ./emacs/undo-tree.nix {};
    usePackage = callPackage ./emacs/use-package.nix {};
    weechat-el = callPackage ./emacs/weechat-el.nix {};

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
  };
}
