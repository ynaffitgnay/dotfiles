# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

## don't put duplicate lines or lines starting with space in the history.
## See bash(1) for more options
#HISTCONTROL=ignoreboth

# don't put duplicate lines in the history
HISTCONTROL=ignoredups

# append to the history file, don't overwrite it
shopt -s histappend
# append the current session history to the content of the history file
PROMPT_COMMAND='history -a'

# define quiet resize
qres() {
  resize &>/dev/null
}

# resize window each with each prompt if using serial application
if [ $(tty) == '/dev/ttyS0' ]; then
  trap qres DEBUG
fi

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
# Leave blank for unlimited history
HISTSIZE=
HISTFILESIZE=

# set emacs as default editor
export VISUAL="emacs -nw"
export EDITOR="$VISUAL"

# set language
export LANG=en_US.UTF-8

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

# Disable Ctrl-s (scroll lock) in terminal
stty -ixon

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color|*-256color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
#force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
	# We have color support; assume it's compliant with Ecma-48
	# (ISO/IEC-6429). (Lack of such support is extremely rare, and such
	# a case would tend to support setf rather than setaf.)
	color_prompt=yes
    else
	color_prompt=
    fi
fi

if [ "$color_prompt" = yes ]; then
    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
fi
## Other prompt that's nice
#export PS1="${debian_chroot:+($debian_chroot)}\A \$? \u@\h:\w\n\\$ \[$(tput sgr0)\]"
# options are here: https://www.gnu.org/software/bash/manual/html_node/Controlling-the-Prompt.html


unset color_prompt force_color_prompt

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
    ;;
*)
    ;;
esac

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# colored GCC warnings and errors
#export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

# some more ls aliases
alias ll="ls -alFh"
# ls -l with most recently modified at top
alias llt="ls -alFht"
alias la="ls -A"
alias l="ls -CF"
alias sl="ls -F"
alias LS="ls -F"

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# alias for grep (show line, ignore binary files, recursive)
alias gr="grep -I -n -r --"

# alias for finding a file in the current subdir
alias f="find . -name"

alias free='free -h'
alias cls='clear'
alias cl="clear;ls;"
alias cd..='cd ..'
alias ..='cd ..'
alias ...='cd ../..'
alias .4='cd ../../../../'
alias .5='cd ../../../../..'
alias tree='tree -Ch'
alias d='du -sch'
alias vi='vim'
alias apt-get='sudo apt-get'
alias mv="mv -v"
alias rm="rm -vi"
alias cp="cp -v"
alias more='view -'
alias slime='screen -S VSlime'
# Force screen to ask you for a session name
alias screen='screen -S'

# Git Aliases
alias gcl='git clone'
alias ga='git add'
alias gall='git add -A'
alias gf='git fetch --all --prune --verbose'
alias gft='git fetch --all --prune --tags --verbose'
alias gus='git reset HEAD'
alias gm="git merge"
alias g='git'
alias gs='git status'
alias gaa="gs; git add -A; gs"
alias gss='git status -s'
alias gsu='git submodule update --init --recursive'
alias gl='git pull'
alias gpr='git pull --rebase'
alias gpp='git pull && git push'
alias gup='git fetch && git rebase'
alias gp='git push'
alias gpo='git push origin'
alias gpu='git push --set-upstream'
alias gpom='git push origin master'
alias gdv='git diff -w "$@" | vim -R -'   # ignore whitespace and open read-only
alias gde=$'git diff -w "$@" | emacs -nw --insert <(cat) </dev/tty --eval \'(setq buffer-read-only t)\''
alias gc='git commit -v'
alias gca='git commit -v -a'
alias gcm='git commit -v -m'
alias gci='git commit --interactive'
alias gb='git branch'
alias gba='git branch -a'
alias gbt='git branch --track'
alias gcount='git shortlog -sn'
alias gcp='git cherry-pick'
alias gco='git checkout'
alias gcb='git checkout -b'
alias gct='git checkout --track'
alias gexport='git archive --format zip --output'
alias gdel='git branch -D'
alias gmu='git fetch origin -v; git fetch upstream -v; git merge upstream/master'
alias gll='git log --graph --pretty=oneline --abbrev-commit'
alias gg="git log --graph --pretty=format:'%C(bold)%h%Creset%C(yellow)%d%Creset %s %C(yellow)%an %C(cyan)%cr%Creset' --abbrev-commit --date=relative"
alias ggs="gg --stat"
alias gsl="git shortlog -sn"
alias gw="git whatchanged"
alias gt="git tag"
alias gta="git tag -a"
alias gtd="git tag -d"
alias gtl="git tag -l"

# see the file history of commits w diffs, including renames
alias ghist="git log --follow -p -- "

# get 10 largest files in the repository
alias g10l="git ls-tree -r -t -l --full-name HEAD | sort -n -k 4 | tail -n 10"

# Currently tracked files that would be ignored given the current .gitignores
alias gignored="git ls-files -i --exclude-standard"  
  
# From http://blogs.atlassian.com/2014/10/advanced-git-aliases/
# Show commits since last pull
alias gnew="git log HEAD@{1}..HEAD@{0}"
# Add uncommitted and unstaged changes to the last commit
alias gcaa="git commit -a --amend -C HEAD"
# Amend commit message
alias gcam="git commit --amend"

# shortcuts for local aliases
alias lba='less ~/.bash_aliases'
alias sba='source ~/.bash_aliases'

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi


case $OSTYPE in
  darwin*)
    alias gtls="git tag -l | gsort -V"
    ;;
  *)
    alias gtls='git tag -l | sort -V'
    ;;
esac

alias gd='git diff'
#if [ -z "$EDITOR" ]; then
    #case $OSTYPE in
      #linux*)
        #alias gd='git diff | vim -R -'
        #;;
      #darwin*)
        #alias gd='git diff | mate'
        #;;
      #*)
        #alias gd='git diff'
        #;;
    #esac
#else
    #alias gd="git diff | $EDITOR"
#fi
# End Git Aliases


# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

#PS1="\[\e[1;35m\]-[\[\e[1;32m\]\h\[\e[1;35m\]]- -[\[\e[1;36m\]\w\[\e[1;35m\]]-\n\[\e[1;35m\]-[\[\e[1;36m\]\@\[\e[1;35m\]]-\[\e[0m\]"

cd ~

# Add RVM to PATH for scripting. Make sure this is the last PATH variable change.
export PATH="$PATH:$HOME/.rvm/bin"
export PATH="$PATH:$HOME/bin"
export PATH="$PATH:/usr/local/bin"
export PATH="$PATH:$HOME/.local/bin"
export PATH="$PATH:$HOME/.scripts"
export GOPATH="$HOME/go"
export GOBIN="$GOPATH/bin"
# Remove duplicates from path
export PATH=$(echo "$PATH" | awk -v RS=':' -v ORS=":" '!a[$1]++{if (NR > 1) printf ORS; printf $a[$1]}')

# LESS man page colors (makes Man pages more readable).
export LESS_TERMCAP_mb=$'\E[01;31m'
export LESS_TERMCAP_md=$'\E[01;31m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_so=$'\E[01;44;33m'
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[01;32m'
