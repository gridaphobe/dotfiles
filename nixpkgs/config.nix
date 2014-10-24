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
          self.structuredHaskellMode
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
          self.ivoryBackendAadl
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
      ivoryBackendAadl   = callPackage ../Source/ivory/ivory-backend-aadl/default.nix {};
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
      ghcMod         = callPackage ./ghc-mod.nix { makeWrapper = makeWrapper; };
      structuredHaskellMode = callPackage ../Source/structured-haskell-mode/default.nix {
        # haskellSrcExts = self.haskellSrcExts_1_15_0_1;
      };

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

    haskellPackages = haskellPackages_wrapper pkgs.haskellPackages;
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
      paths = with vimPlugins; [
        macvim

        align
        airline
        commentary
        easymotion
        fugitive
        ghcmod-vim
        gitgutter
        gundo
        hasksyn
        hoogle
        idris-vim
        neco-ghc
        stylish-haskell
        surround
        syntastic
        tmux-navigator
        vimproc
        vimshell

        vim-rsi
        vim-sensible
      ];
    };

    emacsEnv = pkgs.buildEnv {
      name = "emacs-env";
      paths = [
        emacs

        aspell
        aspellDicts.en

        auctex
        company-mode
        dash-el
        epl
        evil
        evil-god-state
        evil-surround
        exec-path-from-shell
        flycheck
        flycheck-pos-tip
        ghc-mod-el
        git-commit-mode
        git-rebase-mode
        gitattributes-mode
        gitconfig-mode
        gitignore-mode
        god-mode
        haskell-mode
        helm
        magit
        markdown-mode
        # mu4eMaildirsExtension
        org-plus-contrib
        pkg-info-el
        projectile
        rich-minority
        s-el
        smart-mode-line
        smartparens
        structured-haskell-mode-el
        switch-window
        undo-tree
        use-package
        volatile-highlights
        weechat-el
      ];
    };

    mu = pkgs.mu.override { libsoup = (libsoup.override { gnomeSupport = false; }); };

    emacs                      = pkgs.emacs24Macport;
    melpa                      = callPackage ./emacs/melpa.nix {};

    async                      = callPackage ./emacs/async.nix {};
    auctex                     = callPackage ./emacs/auctex.nix {};
    bind-key                   = callPackage ./emacs/bind-key.nix {};
    company-mode               = callPackage ./emacs/company-mode.nix {};
    dash-el                    = callPackage ./emacs/dash.nix {};
    diminish                   = callPackage ./emacs/diminish.nix {};
    evil                       = callPackage ./emacs/evil.nix {};
    evil-god-state             = callPackage ./emacs/evil-god-state.nix {};
    evil-surround              = callPackage ./emacs/evil-surround.nix {};
    flycheck                   = callPackage ./emacs/flycheck.nix {};
    flycheck-pos-tip           = callPackage ./emacs/flycheck-pos-tip.nix {};
    epl                        = callPackage ./emacs/epl.nix {};
    exec-path-from-shell       = callPackage ./emacs/exec-path-from-shell.nix {};
    ghc-mod-el                 = callPackage ./emacs/ghc-mod.nix { 
      ghcMod                   = haskellPackages.ghcMod; 
    };
    git-commit-mode            = callPackage ./emacs/git-commit-mode.nix {};
    git-rebase-mode            = callPackage ./emacs/git-rebase-mode.nix {};
    gitattributes-mode         = callPackage ./emacs/gitattributes-mode.nix {};
    gitconfig-mode             = callPackage ./emacs/gitconfig-mode.nix {};
    gitignore-mode             = callPackage ./emacs/gitignore-mode.nix {};
    god-mode                   = callPackage ./emacs/god-mode.nix {};
    goto-chg                   = callPackage ./emacs/goto-chg.nix {};
    haskell-mode               = callPackage ./emacs/haskell-mode.nix {};
    helm                       = callPackage ./emacs/helm.nix {};
    magit                      = callPackage ./emacs/magit.nix {};
    markdown-mode              = callPackage ./emacs/markdown-mode.nix {};
    # mu4eMaildirsExtension    = callPackage ./emacs/mu4e-maildirs-extension.nix { mu = mu; };
    rich-minority              = callPackage ./emacs/rich-minority.nix {};
    org-plus-contrib           = callPackage ./emacs/org-plus-contrib.nix {};
    popup-el                   = callPackage ./emacs/popup.nix {};
    projectile                 = callPackage ./emacs/projectile.nix {};
    pkg-info-el                = callPackage ./emacs/pkg-info.nix {};
    s-el                       = callPackage ./emacs/s-el.nix {};
    smart-mode-line            = callPackage ./emacs/smart-mode-line.nix {};
    smartparens                = callPackage ./emacs/smartparens.nix {};
    structured-haskell-mode-el = callPackage ./emacs/structured-haskell-mode.nix {
      structuredHaskellMode    = haskellPackages.structuredHaskellMode;
    };
    switch-window              = callPackage ./emacs/switch-window.nix {};
    undo-tree                  = callPackage ./emacs/undo-tree.nix {};
    use-package                = callPackage ./emacs/use-package.nix {};
    volatile-highlights        = callPackage ./emacs/volatile-highlights.nix {};
    weechat-el                 = callPackage ./emacs/weechat-el.nix {};

  };
}
