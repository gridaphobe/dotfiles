{ pkgs }: {
  allowUnfree = true;

  packageOverrides = self: with pkgs; rec {

    devToolsEnv = pkgs.buildEnv {
      name = "dev-tools";
      paths = [
        aspell
        aspellDicts.en
        curl
        emacs
        gitAndTools.gitFull
        gitAndTools.hub
        globalHsEnv
        graphviz
        guile
        htop
        imagemagick
        # macvim
        nodejs
        # ocaml
        ocamlPackages.opam
        p7zip
        parallel
        # plantuml
        perl
        postgresql
        python
        rlwrap
        ruby
        rubyLibs.terminal_notifier
        rust
        scala
        simpleBuildTool
        silver-searcher
        sloccount
        sqlite
        tmux
        tree
        # vimNox
        weechat
        wget
        # z3
        zsh
      ];
    };

    z3 = callPackage ./z3.nix {};

    globalHsEnv = haskellPackages_ghc783.ghcWithPackages (self: [
      self.cabal2nix
      self.cabalInstall
      self.ghcCore
      self.ghcMod
      self.hasktags
      self.hdevtools
      self.hlint
      self.hscolour
      self.myHoogleLocal
      pkgs.emacs24Packages.structuredHaskellMode
      self.stylishHaskell
      self.pandoc
      self.pandocCiteproc
      self.pandocTypes

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
        ocaml  = pkgs.ocaml;
        z3     = pkgs.z3;
      };
      liquidhaskell  = callPackage /Users/gridaphobe/Source/liquid/haskell/cabal.nix {};
      LiquidCheck    = callPackage /Users/gridaphobe/Source/liquid/check/cabal.nix {};
      hdevtools      = callPackage /Users/gridaphobe/Source/hdevtools {};
      systemFileio   = self.disableTest  super.systemFileio;
      shake          = self.disableTest  super.shake;
      intern         = callPackage ./intern.nix {};

      myHoogleLocal  = self.hoogleLocal.override {
        packages = with self; ([
          ghc
          liquidFixpoint
          liquidhaskell
          LiquidCheck
        ] ++ (builtins.filter (p: !(p == pkgs.ocaml || p == pkgs.z3))
                              liquidFixpoint.propagatedNativeBuildInputs)
          ++ liquidhaskell.propagatedNativeBuildInputs
          ++ LiquidCheck.propagatedNativeBuildInputs
        );
      };
    };


    emacs = pkgs.emacs24Macport;
    emacs24Packages = recurseIntoAttrs (emacsPackages emacs24Macport pkgs.emacs24Packages);
    haskellPackages_wrapper = hp: self.recurseIntoAttrs (hp.override {
        extension = this: super: haskellProjects {
          self = this;
          super = super;
          callPackage = self.lib.callPackageWith this;
        };
      });

    haskellPackages_ghc783 = haskellPackages_wrapper self.haskellPackages_ghc783;
    haskellPackages_ghc783_profiling = haskellPackages_wrapper self.haskellPackages_ghc783_profiling;

    hsEnv = { name, ghc, deps }:
      let hsPkgs = ghc.ghcWithPackages (self : ([
          # self.cabal2nix
          self.cabalInstall
          #self.ghcCore
          #self.ghcMod
          #self.hasktags
          #self.HaRe
          #self.hdevtools
          self.hlint
        ] ++ (deps self)));
      in
        pkgs.myEnvFun {
          name = name;
          buildInputs = [
            pkgs.binutils
            pkgs.coreutils
            pkgs.zsh
            hsPkgs
          ];
          shell = "${pkgs.zsh.outPath}/bin/zsh";
          extraCmds = ''
            $(grep export ${hsPkgs.outPath}/bin/ghc)
          '';
        };
  };
}