#!/bin/sh

set -e

srcdir=$(dirname $0)
test -z "$srcdir" && srcdir=.

echo "-- Generating configure script"
autoreconf --verbose --force --install $srcdir

# These files may not be created by older versions of autoconf
if test ! -f $srcdir/config.guess
then
    cp -v $(automake --print-libdir)/config.guess $srcdir
fi

if test ! -f $srcdir/config.sub
then
    cp -v $(automake --print-libdir)/config.sub $srcdir
fi

if test ! -f $srcdir/install-sh
then
    cp -v $(automake --print-libdir)/install-sh $srcdir
fi

if test ! -f $srcdir/Macaulay2/editors/emacs/M2.el
then
    if test -e $srcdir/../.git
    then
	echo "-- Updating M2-emacs submodule"
	git submodule update --init $srcdir/Macaulay2/editors/emacs
    else
	echo "-- Warning: Not in a git repository; unable to update M2-emacs submodule."
	echo "-- You may download it from https://github.com/Macaulay2/M2-emacs and extract"
	echo "-- its contents to the Macaulay2/M2/emacs subdirectory."
    fi
fi
