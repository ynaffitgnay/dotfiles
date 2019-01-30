# append to history file, don't overwrite it
shopt -s histappend
PROMPT_COMMAND='history -a'

# for setting history length see HISTSIZE and HISTFILESIZE
HISTSIZE=1000
HISTFILESIZE=2000

# set emacs as default editor
export VISUAL="emacs -nw"
export EDITOR="$VISUAL"

# Load aliases
if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi
