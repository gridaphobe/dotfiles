{ pkgs }: {
  allowBroken = true;
  allowUnfree = true;

  haskellPackageOverrides = with pkgs.haskell-ng.lib; self: super: {
    liquid-fixpoint = dontCheck (self.callPackage ../Source/liquid/fixpoint {
      inherit (pkgs) ocaml;
    });
    liquidhaskell  = self.callPackage ../Source/liquid/haskell {};
    target         = self.callPackage ../Source/liquid/check {};

    ghci-ng        = self.callPackage ../Source/ghci-ng {};

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

    shell-env = pkgs.buildEnv {
      name = "shell-env";
      paths = [
        # acl2
        arcanist
        autoconf
        automake
        bashInteractive
        cacert
        cmake
        coreutils
        curl
        cvc4
        darcs
        diffutils
        dovecot22
        file
        findutils
        fish
        ghostscript
        gitAndTools.gitFull
        gitAndTools.hub
        gnugrep
        gnumake
        gnupatch
        #gnupg
        gnused
        gnutar
        #graphviz
        imagemagick
        isync
        leafnode
        mu
        # (mutt.override { withSidebar = true;})
        nix-prefetch-scripts
        nix-repl
        # notmuch
        ocaml
        patch
        patchutils
        pkgconfig
        rlwrap
        sbcl
        silver-searcher
        sloccount
        subversion
        terminal-notifier
        tmux
        tree
        #weechat
        wget
        xz
        z3
        zsh
      ];
    };

    haskell-env = (haskellngPackages.ghcWithPackages cabalPackages).overrideDerivation (drv: { 
      name = "haskell-env";
      postBuild = ''
        ${drv.postBuild}
        $out/bin/ghc-pkg expose ghc
      '';
    });

    hoogle-local = lib.overrideDerivation (withHoogle haskell-env) (drv: {
      name = "hoogle-with-packages";
    });

    withHoogle = haskellEnv: with haskellngPackages;
      import <nixpkgs/pkgs/development/libraries/haskell/hoogle/local.nix> {
        inherit stdenv hoogle rehoo ghc;
        packages = haskellEnv.paths;
      };

    cabalPackages = hp: with hp; [
      cabal2nix
      cabal-install
      ghc-core
      ghci-ng
      hakyll
      haskell-docs
      hasktags
      hlint
      hscolour
      structured-haskell-mode
      stylish-haskell
      pandoc
      pandoc-citeproc
      pandoc-types
      shake
      SafeSemaphore
      scotty
      mtl-compat

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
      sexp-show
      data-timeout
      xml-conduit
      toml

      ghc-syb-utils
      th-lift

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

      #OpenGL
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


    vim-env = pkgs.buildEnv {
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

    python-env = python.buildEnv.override {
      extraLibs = with pythonPackages; [ pandas tables ];
      ignoreCollisions = true;
    };
    #python-env = pkgs.buildEnv {
    #  name = "python-env";
    #  paths = with pythonPackages; [
    #    python
    #    ipython
    #    pandas
    #    tables
    #  ];
    #};

    emacs-env = pkgs.buildEnv {
      name = "emacs-env";
      paths = with emacsPackagesNgGen emacs; [
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
        # evil
        # evil-god-state
        # evil-surround
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
        monokai-theme
        org-plus-contrib
        projectile
        smart-mode-line
        smartparens
        switch-window
        # structured-haskell-mode
        undo-tree
        use-package
        volatile-highlights
        wgrep
        zenburn-theme
      ];
    };

    mu = pkgs.mu.override { libsoup = (libsoup.override { gnomeSupport = false; }); };
    notmuch = pkgs.notmuch.override { talloc = (talloc.override { libcap = null; }); };

    emacs = pkgs.emacs24Macport;
  };
}
