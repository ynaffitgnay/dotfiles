This directory contains files that will hopefully ease the process of
maintaining the same development environment across different devices.

This process is kind of hacked together and will hopefully become more
streamlined/ nice over time.

For general setup, you should:
1. set up ~/.emacs.d
   *  Make sure to clone the .emacs.d repository into home. It's a good idea
      to do this first in order to avoid having to deal with having other stuff
      in the directory.

1. If you're on a mac, symbolically link everything. It'll drive you less insane.

1. run setup.sh for each of the modules 
   *  if .bash_profile already existed, make sure that it also starts ssh agent.
      add the following lines to the system's .bash_profile if they're not
      already there
      ```
      if [ -f ~/.bashrc ]; then
  . ~/.bashrc
fi

# get ssh-agent going
# don't want ssh-agent in .bashrc because it prevents scp
eval "$(ssh-agent -s)"
ssh-add

      ```
1. make a ~/.bash_aliases for the local machine
