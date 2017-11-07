#! /usr/bin/env bash

EMACSD=$HOME/.emacs.d

function backup {
    if [ -e $HOME/$1 ]; then
        echo "-- $1 found."
        mv -v $HOME/$1 $HOME/"$1.old"
    fi
}

if [ "$(pgrep -u $USER "^[eE]macs")" ]; then
    echo "-- Emacs is running. Shutdown it and try again."
    exit 1
else
    for oldfile in ".emacs" ".emacs.d"; do
        backup $oldfile
    done

    git clone git@github.com:cbpark/minimacs.d.git ${EMACSD} \
        || git clone https://github.com/cbpark/minimacs.d.git ${EMACSD} \
        || { echo "-- git clone failed."; exit 1; }

    mkdir -p ${EMACSD}/{backup,autosave,etc}

    echo -e "-- Succeesfully done.\n-- Happy Hacking!"
fi
