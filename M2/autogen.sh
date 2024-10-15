#!/bin/sh

set -e

echo "-- Generating configure script"
autoreconf --verbose --force --install

if test ! -f Macaulay2/editors/emacs/M2.el
then
    if test -e ../.git
    then
	echo "-- Updating M2-emacs submodule"
	git submodule update --init Macaulay2/editors/emacs
    else
	echo "-- Warning: Not in a git repository; unable to update M2-emacs submodule."
	echo "-- You may download it from https://github.com/Macaulay2/M2-emacs and extract"
	echo "-- its contents to the Macaulay2/M2/emacs subdirectory."
    fi
fi
