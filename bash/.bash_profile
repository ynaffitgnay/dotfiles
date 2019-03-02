if [ -f ~/.bashrc ]; then
  . ~/.bashrc
fi

cd -

# get ssh-agent going
# don't want ssh-agent in .bashrc because it prevents scp
eval "$(ssh-agent -s)"
ssh-add

# make sure keyboard maps properly over X??
setxkbmap -rules evdev -model pc104 -layout us

test -e "${HOME}/.iterm2_shell_integration.bash" && source "${HOME}/.iterm2_shell_integration.bash"

