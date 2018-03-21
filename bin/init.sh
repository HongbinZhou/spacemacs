#!/bin/bash

whereAmI=$(dirname $(readlink -f $0))

[[ -d ~/.emacs.d ]] && mv ~/.emacs.d ~/.emacs.d.bak
(
    git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d
    cd ~/.emacs.d && git checkout develop
)

