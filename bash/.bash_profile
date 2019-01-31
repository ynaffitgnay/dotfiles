if [ -f ~/.bashrc ]; then
  . ~/.bashrc
fi

# get ssh-agent going (better to do it here)
eval "$(ssh-agent -s)"
ssh-add

# Setting PATH for Python 3.4
# The orginal version is saved in .bash_profile.pysave
PATH="/Library/Frameworks/Python.framework/Versions/3.4/bin:${PATH}"
export PATH

# Setting PATH for Python 3.4
# The orginal version is saved in .bash_profile.pysave
PATH="/Library/Frameworks/Python.framework/Versions/3.4/bin:${PATH}"
export PATH

# added by Miniconda3 3.19.0 installer
export PATH="/Users/tiffany/miniconda3/bin:$PATH"

# added by Anaconda3 2.5.0 installer
export PATH="/Users/tiffany/anaconda/bin:$PATH"


