#!/bin/bash


if [[ "$1" = "gdb" ]]; then \
    gdb -i=mi --args $(which emacs) \
    "-Q" \
    "-L" "~/build/emacs-jupyter-client/src" \
    "-l" "~/repos/emacs-jupyter-client/test/test.el"
else
emacs -Q \
      -L "~/build/emacs-jupyter-client/src" \
      -l "~/repos/emacs-jupyter-client/test/test.el"

fi
