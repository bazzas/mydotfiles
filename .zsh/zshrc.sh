# Vars
	HISTFILE=~/.zsh_history
	SAVEHIST=100000
	setopt inc_append_history # To save every command before it is executed
	setopt share_history # setopt inc_append_history
# Settings
	export VISUAL=vim

	# Virtualenvwrapper settings
	export WORKON_HOME=$HOME/.virtualenvs
	export PROJECT_HOME=$HOME/Devel
	export VIRTUALENVWRAPPER_SCRIPT=/usr/bin/virtualenvwrapper.sh

source /usr/bin/virtualenvwrapper_lazy.sh

source ~/.zsh/plugins/fixls.zsh

# Completions
# These are all the plugin options available: https://github.com/robbyrussell/oh-my-zsh/tree/291e96dcd034750fbe7473482508c08833b168e3/plugins
#
# Edit the array below, or relocate it to ~/.zshrc before anything is sourced
# For help create an issue at github.com/parth/dotfiles

autoload -U compinit

plugins=(
	docker
	virtualenv
	virtualenvwrapper
)

for plugin ($plugins); do
    fpath=(~/.zsh/plugins/oh-my-zsh/plugins/$plugin $fpath)
done

compinit

source ~/.zsh/plugins/oh-my-zsh/lib/history.zsh
source ~/.zsh/plugins/oh-my-zsh/lib/key-bindings.zsh
source ~/.zsh/plugins/oh-my-zsh/lib/completion.zsh
source ~/.zsh/plugins/vi-mode.plugin.zsh
source ~/.zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh
source ~/.zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source ~/.zsh/keybindings.sh
# Fix for arrow-key searching
# start typing + [Up-Arrow] - fuzzy find history forward
if [[ "${terminfo[kcuu1]}" != "" ]]; then
	autoload -U up-line-or-beginning-search
	zle -N up-line-or-beginning-search
	bindkey "${terminfo[kcuu1]}" up-line-or-beginning-search
fi
# start typing + [Down-Arrow] - fuzzy find history backward
if [[ "${terminfo[kcud1]}" != "" ]]; then
	autoload -U down-line-or-beginning-search
	zle -N down-line-or-beginning-search
	bindkey "${terminfo[kcud1]}" down-line-or-beginning-search
fi

source ~/.zsh/prompt.sh
export PATH=$PATH:$HOME/scripts

export W3MIMGDISPLAY_PATH=/usr/libexec/w3m/w3mimgdisplay

# User specific aliases and functions
alias gs='git status '
alias ga='git add '
alias gb='git branch '
alias gc='git commit'
alias gd='git diff'
alias go='git checkout '

alias cp='cp -iv'
alias mv='mv -iv'

# git bare repository to track dotfiles https://www.atlassian.com/git/tutorials/dotfiles
alias config='/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME'

# program to create snapshots
alias myscrot='scrot ~/Pictures/Screenshots/%Y-%m-%dT%H%M%S.png'

# Automation engine aliases

alias s_wh='cd /home/serhii/projects/wss-home-frontend && npm start'
alias s_rw='cd /home/serhii/projects/remote-working-frontend && npm run start:spa'
alias s_sl='cd /home/serhii/projects/ssa-leave-frontend && npm run start:spa'
alias s_em='cd /home/serhii/projects/equipment-assign-frontend && npm run start:spa'

alias shn='shutdown -h now'
