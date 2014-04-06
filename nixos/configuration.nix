# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Use the GRUB 2 boot loader.
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  # Define on which hard drive you want to install Grub.
  boot.loader.grub.device = "/dev/sda";

  networking.hostName = "nixos"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless.

  # Select internationalisation properties.
  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  time.timeZone = "America/Los_Angeles";
  
  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable the X11 windowing system.
  services.xserver.enable = true;
  services.xserver.layout = "us";
  services.xserver.xkbOptions = "ctrl:nocaps";

  services.xserver.windowManager.xmonad.enable = true;
  services.xserver.windowManager.xmonad.enableContribAndExtras = true;
  services.xserver.windowManager.default = "xmonad";
  services.xserver.desktopManager.xfce.enable = true;
  services.xserver.desktopManager.default = "xfce";

  services.dovecot2.enable = true;
  services.avahi.enable = true;
  services.avahi.nssmdns = true;

  # Users
  users.extraUsers.gridaphobe = {
    createHome = true;
    home = "/home/gridaphobe";
    description = "Eric Seidel";
    extraGroups = [ "wheel" ];
    useDefaultShell = true;
    uid = 1000;
  };

  # Packages
  environment.systemPackages = with pkgs; [
    vimHugeX
    (pkgs.lib.overrideDerivation emacs (attrs: {
      name = "emacs-HEAD";
      src = /home/gridaphobe/src/emacs;
      buildInputs = pkgs.emacs.buildInputs ++ [ automake autoconf imagemagickBig ];
      configureFlags = pkgs.emacs.configureFlags
                    ++ [ "--with-xml2" "--with-imagemagick" "--with-file-notification=inotify" ];
      doCheck = false;
    }))
    git
    bazaar
    fish
    zsh
    haskellPackages.ghc
    haskellPackages.cabalInstall_1_18_0_3
    z3
    ocaml_4_01_0
    
    haskellPackages.xmobar
    dmenu
    chromium
    firefox

    gcc
    automake
    autoconf
    gnumake
    curl
    wget
    zlib
    unzip
    file
    ruby
    python
  ];

  # Options
  programs.bash.enableCompletion = true;
  fonts.enableCoreFonts = true;
  fonts.enableFontDir = true;
  fonts.enableGhostscriptFonts = true;
  fonts.extraFonts = with pkgs; [ terminus_font ];

  # Overrides
  nixpkgs.config.packageOverrides = pkgs: {
    emacs = pkgs.emacs.override { 
      gtk = pkgs.gtk3;
    };
    z3 = pkgs.callPackage ./z3.nix {};
  };
}
