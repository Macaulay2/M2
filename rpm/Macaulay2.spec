Name: Macaulay2
Version: 0.9.95
Release: 2
Summary: Macaulay 2 is a software system for algebraic geometry research.
License: GNU General Public License, version 2
# Distribution: Debian
Group: Mathematics
#Group: Converted/math
Buildroot: /home/dan/src/M2/BUILD/normal/rpm/macaulay2-0.9.95
# BuildRoot: /capybara/encap/%{Name}-%{Version}

%define _rpmdir ../../
%define _rpmfilename %%{NAME}-%%{VERSION}-%%{ARCH}-Linux.rpm
%define _unpackaged_files_terminate_build 1

%post
#!/bin/sh -e
prefix=/usr/local
case "$1" in
    abort-upgrade|configure)
	$prefix/bin/M2 --dumpdata
	for p in $prefix/share/Macaulay2/*.m2
	do f="$prefix/share/info/`basename $p .m2`.info"
	   if [ -f $f ] ; then install-info --quiet --info-dir="$prefix/share/info" "$f" ; fi
	done ;;
    abort-remove|abort-deconfigure) ;;
    *) echo "postinst called with unknown argument \`$1'" >&2
       exit 1 ;;
esac


%preun
#! /bin/sh -e
prefix=/usr/local
case "$1" in
    remove|deconfigure|upgrade)
	rm -f $prefix/lib/Macaulay2/Core/cache/*
	for p in $prefix/share/Macaulay2/*.m2
	do f="$prefix/share/info/`basename $p .m2`.info"
	   if [ -f $f ] ; then install-info --remove --quiet --info-dir="$prefix/share/info" "$f" ; fi
	done ;;
    failed-upgrade) ;;
    *) echo "prerm called with unknown argument \`$1'" >&2
       exit 1 ;;
esac

%description
Macaulay 2 is a software system for algebraic geometry research, written by
Daniel R. Grayson and Michael E. Stillman.  Based on Groebner bases, it
provides algorithms for computing homological invariants of rings and
modules.

%files
%dir "/usr/local/"
%dir "/usr/local/Macaulay2-0.9.95/"
%dir "/usr/local/Macaulay2-0.9.95/share/"
%dir "/usr/local/Macaulay2-0.9.95/share/doc/"
%dir "/usr/local/Macaulay2-0.9.95/share/doc/Macaulay2/"
%dir "/usr/local/Macaulay2-0.9.95/share/doc/Macaulay2/Core/"
"/usr/local/Macaulay2-0.9.95/share/doc/Macaulay2/Core/COPYING"
"/usr/local/Macaulay2-0.9.95/share/doc/Macaulay2/Core/CHANGES"
"/usr/local/Macaulay2-0.9.95/share/doc/Macaulay2/Core/LAYOUT"
%dir "/usr/local/Macaulay2-0.9.95/share/doc/Macaulay2/Core/emacs/"
"/usr/local/Macaulay2-0.9.95/share/doc/Macaulay2/Core/emacs/emacs-hlp.txt"
"/usr/local/Macaulay2-0.9.95/share/doc/Macaulay2/Core/emacs/emacs.m2"
"/usr/local/Macaulay2-0.9.95/share/doc/Macaulay2/Core/emacs/README"
%dir "/usr/local/Macaulay2-0.9.95/share/doc/Macaulay2/Macaulay2/"
%dir "/usr/local/Macaulay2-0.9.95/share/doc/Macaulay2/Macaulay2/examples/"
"/usr/local/Macaulay2-0.9.95/share/doc/Macaulay2/Macaulay2/examples/.linkdir"
"/usr/local/Macaulay2-0.9.95/share/doc/Macaulay2/Macaulay2/examples/_random_lp__R__R_rp.m2"
"/usr/local/Macaulay2-0.9.95/share/doc/Macaulay2/Macaulay2/examples/_betti_lp__Matrix_rp.m2"
"/usr/local/Macaulay2-0.9.95/share/doc/Macaulay2/Macaulay2/examples/_accumulate_lp__Function_cm__Visible__List_rp.m2"

# ...

# Local Variables:
# compile-command: "make -C $M2BUILDDIR/rpm "
# End:
