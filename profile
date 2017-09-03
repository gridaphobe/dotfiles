
export HTTP_PROXY="http://127.0.0.1:8888/"
export http_proxy="http://127.0.0.1:8888/"
export HTTPS_PROXY="http://127.0.0.1:8888/"
export https_proxy="http://127.0.0.1:8888/"

if [ -d "$HOME/.local/bin" ]; then
    export PATH="$HOME/.local/bin:$PATH"
fi

if [ -d "$HOME/.cabal/bin" ]; then
    export PATH="$HOME/.cabal/bin:$PATH"
fi

if [ -d "$HOME/.cargo/bin" ]; then
    export PATH="$HOME/.cargo/bin:$PATH"
fi

if [ -d "$HOME/bin" ]; then
    export PATH="$HOME/bin:$PATH"
fi

export EDITOR='emacsclient -t -a ""'

# Local Variables:
# mode: sh
# End:
