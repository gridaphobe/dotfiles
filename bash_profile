# -*- mode: sh -*-

[[ -r ~/.profile ]] && . ~/.profile
[[ -r ~/.bashrc ]] && . ~/.bashrc

if [ -e "$HOME/.nix-profile/etc/profile.d/nix.sh" ]; then . "$HOME/.nix-profile/etc/profile.d/nix.sh"; fi # added by Nix installer
NIX_PATH=nixpkgs=/Users/gridaphobe/Source/nixpkgs
