if [ $COLORTERM = 'gnome-terminal' ]
    set -x TERM 'xterm-256color'
end

set -x BROWSER /usr/bin/chromium
set -x PATH $HOME/.cabal/bin $PATH
set -x PATH (ruby -rubygems -e "puts Gem.user_dir")/bin $PATH
set -x PATH $HOME/bin $PATH
set -x EDITOR 'emacsclient -t -a ""'

set -x DJS_DIR $HOME/Source/djs
