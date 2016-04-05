# -*- mode: sh -*-

[[ -r ~/.profile ]] && . ~/.profile
[[ -r ~/.bashrc ]] && . ~/.bashrc

if [[ -r "$HOME/.nix-profile/etc/profile.d/nix.sh" ]]; then
  . "$HOME/.nix-profile/etc/profile.d/nix.sh"
  export NIX_CONF_DIR="$HOME/.config/nix"
  export NIX_PATH=nixpkgs="$HOME/Source/nixpkgs"
  #export CFLAGS="$CFLAGS -I$HOME/.nix-profile/include"
  #export CXXFLAGS="$CXXFLAGS -I$HOME/.nix-profile/include"
  #export LDFLAGS="$LDFLAGS -L$HOME/.nix-profile/lib"
  #export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:$HOME/.nix-profile/lib"
  #export DYLD_LIBRARY_PATH="$DYLD_LIBRARY_PATH:$HOME/.nix-profile/lib"
fi

if [ -e ~/.nix-profile/bin/ghc ]; then
  export NIX_GHC="$HOME/.nix-profile/bin/ghc"
  export NIX_GHCPKG="$HOME/.nix-profile/bin/ghc-pkg"
  export NIX_GHC_DOCDIR="$HOME/.nix-profile/share/doc/ghc/html"
  export NIX_GHC_LIBDIR="$HOME/.nix-profile/lib/ghc-$($NIX_GHC --numeric-version)"
fi

# export PATH="$HOME/Source/arcanist/bin:$PATH" # until i can build php in nixpkgs again..

# # workaround for nixpkgs#6390
# export NIX_CFLAGS_COMPILE="-idirafter /usr/include"
# export NIX_CFLAGS_LINK="-L /usr/lib"
