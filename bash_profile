# -*- mode: sh -*-

[[ -r ~/.profile ]] && . ~/.profile
[[ -r ~/.bashrc ]] && . ~/.bashrc

if [ -e "$HOME/.nix-profile/etc/profile.d/nix.sh" ]; then . "$HOME/.nix-profile/etc/profile.d/nix.sh"; fi # added by Nix installer

export NIX_CONF_DIR="$HOME/.config/nix"
export NIX_REMOTE_SYSTEMS="$HOME/.config/nix/remote-systems.conf"
#export NIX_BUILD_HOOK="$HOME/.nix-profile/libexec/nix/build-remote.pl"
#export NIX_CURRENT_LOAD=/tmp/current-load
export NIX_PATH=nixpkgs="$HOME/Source/nixpkgs"
# export CFLAGS="$CFLAGS -I$HOME/.nix-profile/include"
# export CXXFLAGS="$CXXFLAGS -I$HOME/.nix-profile/include"
# export LDFLAGS="$LDFLAGS -L$HOME/.nix-profile/lib"
# export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:$HOME/.nix-profile/lib"
# export DYLD_LIBRARY_PATH="$DYLD_LIBRARY_PATH:$HOME/.nix-profile/lib"
export NIX_GHC="$HOME/.nix-profile/bin/ghc"
export NIX_GHCPKG="$HOME/.nix-profile/bin/ghc-pkg"
export NIX_GHC_DOCDIR="$HOME/.nix-profile/share/doc/ghc/html"
export NIX_GHC_LIBDIR="$HOME/.nix-profile/lib/ghc-$($NIX_GHC --numeric-version)"
