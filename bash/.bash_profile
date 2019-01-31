if [ -f ~/.bashrc ]; then
  . ~/.bashrc
fi

# get ssh-agent going
# don't want ssh-agent in .bashrc because it prevents scp
eval "$(ssh-agent -s)"
ssh-add
