# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
        . "$HOME/.bashrc"
    fi
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

export PATH="$HOME/local/texlive/2010/bin/x86_64-linux:$HOME/.cabal/bin:/usr/local/bin:$PATH"
export GREP_OPTIONS='--color=auto --ignore-case'
export LIBRARY="$HOME/Library"
export GEM_HOME="$HOME/.gems"
export SSH_AUTH_SOCK="$GNOME_KEYRING_CONTROL/ssh"
