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
