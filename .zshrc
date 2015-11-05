HISTSIZE=10000
SAVEHIST=10000
HISTFILE=~/.zsh_history

deletebranch () {
    git branch -D "$@" && git push --delete origin "$@"
}

bindkey -v
bindkey '^R' history-incremental-search-backward

export PS1='%m[%~]☞  '

alias cp='cp -a'
alias ls='ls -AG'
alias gpush='git push'
alias gpushf='git push --force-with-lease'
alias gpull='git pull'
alias gc='git commit -am'
alias gcam='git commit -a --amend'
alias gco='git checkout'
alias gcob='git checkout -b'
alias greb='git pull --rebase origin master'
alias gst='git status'
alias gri='git rebase -i'
alias grhead='git reset --hard HEAD'
alias gprogress='git log --author="Josh Chorlton" --after="1 week ago" --oneline'
alias db='deletebranch'

alias ts='tmux new -s '
alias ta='tmux a -t '
alias tls='tmux ls'

alias cbn="git rev-parse --abbrev-ref HEAD | pbcopy"
