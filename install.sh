#!/bin/bash

emacs -batch -f batch-byte-compile taskpaper-mode.el
mv taskpaper-mode.elc ~/lib/emacs/taskpaper-mode.elc

