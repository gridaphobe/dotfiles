# -*- mode: sh -*-

[[ -r ~/.profile ]] && . ~/.profile
[[ -r ~/.bashrc ]] && . ~/.bashrc

if [ -e "$HOME/.nix-profile/etc/profile.d/nix.sh" ]; then . "$HOME/.nix-profile/etc/profile.d/nix.sh"; fi # added by Nix installer

export NIX_CONF_DIR="$HOME/.config/nix"
export NIX_PATH=nixpkgs="$HOME/Source/nixpkgs"
export CFLAGS="$CFLAGS -I$HOME/.nix-profile/include"
export CXXFLAGS="$CXXFLAGS -I$HOME/.nix-profile/include"
export LDFLAGS="$LDFLAGS -L$HOME/.nix-profile/lib"
export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:$HOME/.nix-profile/lib"
export DYLD_LIBRARY_PATH="$DYLD_LIBRARY_PATH:$HOME/.nix-profile/lib"

if [[ -r "$HOME/.nix-profile/bin/ghc" ]]; then
  eval "$(grep export ~/.nix-profile/bin/ghc)"
fi
