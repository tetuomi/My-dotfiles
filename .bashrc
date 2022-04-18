# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=5000
HISTFILESIZE=6000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

PS1='\n${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '

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
alias sl='ls'
alias ks='ls'
alias k='ls'
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'
alias s='ls -CF'
alias cdw='cd ~/works'
alias cwd='cdw'
alias cdp='cd ../'
alias cpd='cd ../'
alias cdpp='cd ../../'
alias cpdp='cd ../../'
alias cds='cd -'
alias cdd='cd ${HOME}/dotfiles'
alias rm='trash-put'
alias em='emacs'
alias ew='emacs -nw'
alias ec='emacsclient ./'
alias cm='catkin_make'
alias clipboard='xsel --clipboard --input'
alias gpp='g++'
alias g11='g++ -std=c++11'
alias g14='g++ -std=c++14'
alias g17='g++ -std=c++17'
alias bld='mkdir build -p ;cd build ;cmake .. ;make ;cd -'
alias pip='pip3'
alias pf='pip3 freeze'
alias py='python3'
alias py2='python2.7'
alias python='python3'
alias rls='rails'
alias ble='bundle'
alias arduino='cd ${HOME}/Documents/arduino-1.8.5 ;source arduino ;cd -'
alias eixt='exit'
alias shutdown='shutdown -h now'
alias make='make -j4'
alias refresh='. ~/.bashrc'

# trash-cli
# Clone from 'https://github.com/andreafrancia/trash-cli'
if type trash-put &> /dev/null
then
    alias rm=trash-put
fi


# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# Functions about ssh
function ssh-activate() {
    if [ $# -ne 1 ]; then
        echo "Prease set one ssh-key"
        return
    fi
    eval `ssh-agent`
    ssh-add $1
}

function cd() {
    builtin cd $@ &&
    if [ -f "Pipfile" ] ; then
        pipenv shell
    else
        ls -G;
    fi
}

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

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

export EDITOR='emacs'

# Ruby on Rails
# export PATH=$HOME/.rbenv/bin:$PATH
# eval "$(rbenv init -)"
# export PATH="$HOME/.rbenv/plugins/ruby-build/bin:$PATH"

# For point_cloud_viewer
# source $HOME/.cargo/env
# export PATH="$HOME/.cargo/bin:$PATH"

# Greeting
echo -e "\e[32;1m${USER}@${HOSTNAME}\e[m:\e[34;1m~\e[m$"
echo -e "\e[1m Hi, ${USER} !!\e[m"

# export PATH=$HOME/.rbenv/bin:$PATH
# eval "$(rbenv init -)"
# export PATH="$HOME/.rbenv/plugins/ruby-build/bin:$PATH"

# export PATH="/home/tetsu/.pyenv/bin:$PATH"
# eval "$(pyenv init - zsh --no-rehash)"
# eval "$(pyenv virtualenv-init -)"

# Flutter
# export PATH="$PATH:$HOME/flutter/bin"
