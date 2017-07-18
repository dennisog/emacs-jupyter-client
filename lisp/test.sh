#!/bin/bash

# emacs -Q --batch \
#       -L "~/build/emacs-jupyter-client/src" \
#       -l "~/repos/emacs-jupyter-client/lisp/test.el"

gdb -i=mi --args $(which emacs) \
    "-Q" "--batch" \
    "-L" "~/build/emacs-jupyter-client/src" \
    "-l" "~/repos/emacs-jupyter-client/lisp/test.el"
