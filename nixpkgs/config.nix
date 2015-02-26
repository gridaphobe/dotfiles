{ pkgs }: {
  allowBroken = true;
  allowUnfree = true;

  haskellPackageOverrides = with pkgs.haskell-ng.lib; self: super: {
    liquid-fixpoint = dontCheck (self.callPackage ../Source/liquid/fixpoint {
      inherit (pkgs) ocaml;
    });
    liquidhaskell  = self.callPackage ../Source/liquid/haskell {};
    target         = self.callPackage ../Source/liquid/check {};

    ghci-ng        = self.callPackage ../Source/ghci-ng {
    };

    Chart = doJailbreak super.Chart;
    Chart-diagrams = doJailbreak super.Chart-diagrams;
    diagrams-postscript = doJailbreak super.diagrams-postscript;

    enclosed-exceptions = dontCheck super.enclosed-exceptions;
    shake = dontCheck super.shake;

    # ivory           = self.callPackage ../Source/ivory/ivory {};
    # ivory-artifact   = self.callPackage ../Source/ivory/ivory-artifact {};
    # ivory-backend-acl2   = self.callPackage ../Source/ivory/ivory-backend-acl2 {};
    # ivory-backend-c   = self.callPackage ../Source/ivory/ivory-backend-c {};
    # ivory-eval       = self.callPackage ../Source/ivory/ivory-eval {};
    # ivory-examples   = self.callPackage ../Source/ivory/ivory-examples {};
    # ivory-hw         = self.callPackage ../Source/ivory/ivory-hw {};
    # ivory-model-check = self.callPackage ../Source/ivory/ivory-model-check {};
    # ivory-opts       = self.callPackage ../Source/ivory/ivory-opts {};
    # ivory-quickcheck = self.callPackage ../Source/ivory/ivory-quickcheck {};
    # ivory-serialize  = self.callPackage ../Source/ivory/ivory-serialize {};
    # ivory-stdlib     = self.callPackage ../Source/ivory/ivory-stdlib {};

    # tower           = self.callPackage ../Source/tower/tower {};
    # tower-aadl       = self.callPackage ../Source/tower/tower-aadl {};
    # tower-config     = self.callPackage ../Source/tower/tower-config {};
    # tower-statemachine = self.callPackage ../Source/tower/tower-statemachine {};

    # ghc-srcspan-plugin = self.callPackage ../Source/ghc-srcspan-plugin {};

    #hoogleLocal = self.hoogleLocal.override {
    #  packages  = super.cabalPackages self;
    #};
  };

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
        #cacert
        coreutils
        curl
        cvc4
        diffutils
        dovecot22
        findutils
        fish
        gitAndTools.gitFull
        gitAndTools.hub
        gnugrep
        gnumake
        gnupatch
        gnupg
        gnused
        gnutar
        isync
        leafnode
        #mu
        # (mutt.override { withSidebar = true;})
        nix-prefetch-scripts
        #notmuch
        patchutils
        pkgconfig
        rlwrap
        # rubyPackages.terminal-notifier
        # sbcl
        silver-searcher
        sloccount
        subversion
        tmux
        tree
        #weechat
        wget
        z3
        zsh
      ];
    };

    # cvc4 = callPackage ./cvc4.nix {};
    # xapian = callPackage ./xapian.nix {};
    # z3 = callPackage ./z3.nix {};

    haskellEnv = (haskellngPackages.ghcWithPackages cabalPackages).overrideDerivation 
     (drv: { postBuild = ''
      ${drv.postBuild}
      $out/bin/ghc-pkg expose ghc
    '';});

    cabalPackages = hp: with hp; [
      cabal2nix
      cabal-install
      ghc-core
      # ghc-mod
      ghci-ng
      # hakyll
      haskell-docs
      hasktags
      #hdevtools
      #hlint
      hscolour
      # structured-haskell-mode
      stylish-haskell
      pandoc
      pandoc-citeproc
      pandoc-types
      shake
      SafeSemaphore
      scotty

      ad
      data-reify
      lens
      trifecta
      binary-bits

      #cartel
      doctest
      hackage-db
      hspec
      prettyclass
      regex-posix
      Chart
      Chart-diagrams
      text
      pretty-show
      data-timeout
      xml-conduit
      toml

      ghc-syb-utils

      QuickCheck
      smallcheck
      criterion
      tasty
      tasty-hunit
      tasty-quickcheck
      tasty-rerun

      liquid-fixpoint
      liquidhaskell
      optparse-applicative
      target

      OpenGL
      #GLFW
      # ivory
      # ivory-artifact
      # ivory-backend-c
      # ivory-examples
      # ivory-eval
      # ivory-hw
      # ivory-model-check
      # ivory-opts
      # ivory-quickcheck
      # ivory-serialize
      # ivory-stdlib
      # language-c-quote
      # tower
      # tower-aadl
      # tower-config
      # tower-statemachine
      wl-pprint
    ];

    # hoogleLocal = haskellngPackages.hoogleLocal;

    haskellFilterSource = paths: src: builtins.filterSource (path: type:
        let baseName = baseNameOf path; in
        !( type == "unknown"
        || builtins.elem baseName ([".git" ".cabal-sandbox" "dist"] ++ paths)
        || stdenv.lib.hasSuffix ".hi" path
        || stdenv.lib.hasSuffix ".hi-boot" path
        || stdenv.lib.hasSuffix ".o" path
        || stdenv.lib.hasSuffix ".o-boot" path
        || stdenv.lib.hasSuffix ".dyn_o" path
        || stdenv.lib.hasSuffix ".p_o" path))
      src;


    # haskellPackages_wrapper = hp: recurseIntoAttrs (hp.override {
    #     extension = this: super: haskellProjects {
    #       self = this;
    #       super = super;
    #       callPackage = lib.callPackageWith this;
    #     };
    #   });

    # haskellPackages = haskellPackages_wrapper pkgs.haskellngPackages;
    # haskellPackages_ghc784 = haskellPackages_wrapper pkgs.haskellPackages_ghc784;
    # haskellPackages_ghc784_profiling = haskellPackages_wrapper pkgs.haskellPackages_ghc784_profiling;

    # # Define own GHC HEAD package pointing to local checkout.
    # packages_ghcHEAD = pkgs.haskell.packages {
    #   ghcPath = ../Source/ghc;
    #   ghcBinary = pkgs.haskellPackages.ghcPlain;
    #   prefFun = pkgs.haskell.ghcHEADPrefs;
    # };

    # haskellPackages_ghcHEAD = haskellPackages_wrapper packages_ghcHEAD;
    # haskellPackages_ghcHEAD_profiling = haskellPackages_wrapper packages_ghcHEAD.profiling;


    vimEnv = pkgs.buildEnv {
      name = "vim-env";
      paths = with vimPlugins; [
        macvim

        align
        airline
        commentary
        easymotion
        fugitive
        # ghcmod-vim
        gitgutter
        # gundo
        hasksyn
        hoogle
        idris-vim
        neco-ghc
        stylish-haskell
        surround
        syntastic
        tmux-navigator
        vimproc
        #vimrsi
        #vimsensible
        vimshell
      ];
    };

    emacsEnv = pkgs.buildEnv {
      name = "emacs-env";
      paths = with emacsMelpa; [
        emacs

        aspell
        aspellDicts.en

        ac-haskell-process
        ace-jump-mode
        ag
        auctex
        auto-complete
        change-inner
        circe
        company
        evil
        evil-god-state
        evil-surround
        exec-path-from-shell
        expand-region
        flycheck
        flycheck-pos-tip
        gnus
        god-mode
        haskell-mode
        helm
        helm-swoop
        hi2
        idris-mode
        magit
        markdown-mode
        org-plus-contrib
        projectile
        smart-mode-line
        smartparens
        switch-window
        undo-tree
        use-package
        volatile-highlights
        wgrep
      ];
    };

    mu = pkgs.mu.override { libsoup = (libsoup.override { gnomeSupport = false; }); };
    notmuch = pkgs.notmuch.override { talloc = (talloc.override { libcap = null; }); };

    emacs                      = pkgs.emacs24Macport;
  #   melpa                      = callPackage ./emacs/melpa.nix {};

  #   ace-isearch                = callPackage ./emacs/ace-isearch.nix {};
  #   ace-jump-mode              = callPackage ./emacs/ace-jump-mode.nix {};
  #   ag-el                      = callPackage ./emacs/ag.nix {};
  #   async                      = callPackage ./emacs/async.nix {};
  #   auctex                     = callPackage ./emacs/auctex.nix {};
  #   bind-key                   = callPackage ./emacs/bind-key.nix {};
  #   change-inner               = callPackage ./emacs/change-inner.nix {};
  #   circe                      = callPackage ./emacs/circe.nix {};
  #   lcs                        = callPackage ./emacs/lcs.nix {};
  #   lui                        = callPackage ./emacs/lui.nix {};
  #   shorten                    = callPackage ./emacs/shorten.nix {};
  #   tracking                   = callPackage ./emacs/tracking.nix {};
  #   company-mode               = callPackage ./emacs/company-mode.nix {};
  #   dash-el                    = callPackage ./emacs/dash.nix {};
  #   diminish                   = callPackage ./emacs/diminish.nix {};
  #   evil                       = callPackage ./emacs/evil.nix {};
  #   evil-god-state             = callPackage ./emacs/evil-god-state.nix {};
  #   evil-surround              = callPackage ./emacs/evil-surround.nix {};
  #   flycheck                   = callPackage ./emacs/flycheck.nix {};
  #   flycheck-pos-tip           = callPackage ./emacs/flycheck-pos-tip.nix {};
  #   epl                        = callPackage ./emacs/epl.nix {};
  #   exec-path-from-shell       = callPackage ./emacs/exec-path-from-shell.nix {};
  #   expand-region              = callPackage ./emacs/expand-region.nix {};
  #   ghc-mod-el                 = callPackage ./emacs/ghc-mod.nix { 
  #     ghcMod                   = haskellPackages.ghcMod; 
  #   };
  #   git-commit-mode            = callPackage ./emacs/git-commit-mode.nix {};
  #   git-rebase-mode            = callPackage ./emacs/git-rebase-mode.nix {};
  #   gitattributes-mode         = callPackage ./emacs/gitattributes-mode.nix {};
  #   gitconfig-mode             = callPackage ./emacs/gitconfig-mode.nix {};
  #   gitignore-mode             = callPackage ./emacs/gitignore-mode.nix {};
  #   gnus                       = callPackage ./emacs/gnus.nix {};
  #   god-mode                   = callPackage ./emacs/god-mode.nix {};
  #   goto-chg                   = callPackage ./emacs/goto-chg.nix {};
  #   haskell-mode               = callPackage ./emacs/haskell-mode.nix {};
  #   helm                       = callPackage ./emacs/helm.nix {};
  #   helm-swoop                 = callPackage ./emacs/helm-swoop.nix {};
  #   idris-mode                 = callPackage ./emacs/idris-mode.nix {};
  #   magit                      = callPackage ./emacs/magit.nix {};
  #   markdown-mode              = callPackage ./emacs/markdown-mode.nix {};
  #   # mu4eMaildirsExtension    = callPackage ./emacs/mu4e-maildirs-extension.nix { mu = mu; };
  #   rich-minority              = callPackage ./emacs/rich-minority.nix {};
  #   org-plus-contrib           = callPackage ./emacs/org-plus-contrib.nix {};
  #   popup-el                   = callPackage ./emacs/popup.nix {};
  #   projectile                 = callPackage ./emacs/projectile.nix {};
  #   pkg-info-el                = callPackage ./emacs/pkg-info.nix {};
  #   s-el                       = callPackage ./emacs/s-el.nix {};
  #   smart-mode-line            = callPackage ./emacs/smart-mode-line.nix {};
  #   smartparens                = callPackage ./emacs/smartparens.nix {};
  #   # structured-haskell-mode-el = callPackage ./emacs/structured-haskell-mode.nix {
  #   #   structuredHaskellMode    = haskellngPackages.structuredHaskellMode;
  #   # };
  #   switch-window              = callPackage ./emacs/switch-window.nix {};
  #   undo-tree                  = callPackage ./emacs/undo-tree.nix {};
  #   use-package                = callPackage ./emacs/use-package.nix {};
  #   volatile-highlights        = callPackage ./emacs/volatile-highlights.nix {};
  #   weechat-el                 = callPackage ./emacs/weechat-el.nix {};
  #   wgrep                      = callPackage ./emacs/wgrep.nix {};
  };
}
