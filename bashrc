#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
PS1='[\u@\h \W]\$ '

export BROWSER=/usr/bin/chromium
export PATH="$HOME/.cabal/bin:$PATH"
export PATH="$(ruby -rubygems -e "puts Gem.user_dir")/bin:$PATH"
export PATH="$HOME/bin:$PATH"
export EDITOR='emacsclient -t -a ""'
