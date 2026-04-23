#!/usr/bin/env bash

sbcl --noinform --load package.lisp --eval '(save-lisp-and-die "ocflcl" :toplevel (function ocflcl:main) :executable t :compression t :save-runtime-options t)'
