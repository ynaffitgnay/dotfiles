# append to history file, don't overwrite it
shopt -s histappend
PROMPT_COMMAND='history -a'

# for setting history length see HISTSIZE and HISTFILESIZE
HISTSIZE=1000
HISTFILESIZE=2000

# set emacs as default editor
export VISUAL="emacs -nw"
export EDITOR="$VISUAL"

# have confirmation for remove
alias rm="rm -i"

# alias for grep (show line, ignore binary files, recursive)
alias gr='grep -I -n -r --'

# alias for finding a file in the current subdir
alias f="find . -name"

# aliases for git
alias gs="git status"
alias gaa="gs; git add -A; gs"
alias gcm="git commit -m"

################################################################################
#                                                                              #
#                                  MAC ONLY                                    #
#                                                                              #
################################################################################

# alias to copy working directory (with escape characters)
# alias cwd='printf "%q" "$(pwd)" | pbcopy'

################################################################################
#                                                                              #
#                                 LINUX ONLY                                   #
#                                                                              #
################################################################################


################################################################################
#                                                                              #
#                                LOCAL ALIASES                                 #
#                                                                              #
################################################################################
