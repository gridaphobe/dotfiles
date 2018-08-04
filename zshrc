autoload compinit
compinit -Ux

source ~/.antigen.zsh
# antigen use oh-my-zsh
antigen use prezto

antigen bundle command-not-found
antigen bundle emacs
antigen bundle git
antigen bundle github
antigen bundle osx

antigen bundle zsh-users/zsh-syntax-highlighting
antigen bundle zsh-users/zsh-history-substring-search
antigen bundle zsh-users/zsh-autosuggestions
antigen bundle zsh-users/zsh-completions

antigen bundle mafredri/zsh-async
antigen bundle sindresorhus/pure

antigen apply

bindkey -M emacs '^P' history-substring-search-up
bindkey -M emacs '^N' history-substring-search-down

