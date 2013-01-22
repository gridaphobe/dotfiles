# -*- mode: sh -*-

[[ -r /etc/chromium/default ]] && . /etc/chromium/default
export BROWSER=/usr/bin/chromium
export CHROMIUM_USER_FLAGS="$CHROMIUM_FLAGS --memory-model=low --proxy-pac-url=\"http://webproxy.ucsd.edu/proxy.pac\""
export PATH="/usr/local/sbin:$PATH"
export PATH="/usr/local/bin:$PATH"
export PATH="/usr/texbin:$PATH"
export PATH="$HOME/.cabal/bin:$PATH"
export PATH="$(ruby -rubygems -e "puts Gem.user_dir")/bin:$PATH"
export PATH="$HOME/bin:$PATH"
export EDITOR='emacsclient -t -a ""'
