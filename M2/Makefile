# -*- makefile-gmake -*-

.PHONY : all monitor check-make check-autoconf

.NOTPARALLEL:

VERBOSE = yes

all: check-make make-configure monitor include/M2/config.h.in.stamp check-automake

AUTOCONF_MAJOR = 2
AUTOCONF_MINOR = 68
# version 2.60 was the first one with the macro AC_PROG_MKDIR_P
# version 2.62 is required by the configure.ac in gc/libatomic_ops
# version 2.68 is required by mpir 2.6.0
AUTOCONF_VERSION = $(AUTOCONF_MAJOR).$(AUTOCONF_MINOR)

AUTOMAKE_MAJOR = 1
AUTOMAKE_MINOR = 12
# version 1.12 was the first one with the macro AM_PROG_AR, used by memtailor
AUTOMAKE_VERSION = $(AUTOMAKE_MAJOR).$(AUTOMAKE_MINOR)

export PATH := $(shell pwd)/usr-build/bin:$(PATH)

check-make:
	@ echo "$(.FEATURES)" | grep order-only >/dev/null || (echo error: version >= 3.81 of GNU make is required >&2; false)
	@ echo GNU make version okay

make-configure : check-autoconf configure
configure : configure.ac config/files config/ax_func_accept_argtypes.m4 config/ax_prog_cc_for_build.m4 # aclocal.m4
	@ set -x ; autoconf

get-tools: usr-build/aclocal-path get-m4 get-pkg get-autoconf get-automake get-libtool usr-build/aclocal-path2 which-tools
which-tools:
	for i in m4 autoconf automake; do which $$i ; $$i --version | head -1 ; done
	cat usr-build/aclocal-path
	cat usr-build/aclocal-path2
	aclocal --print
usr-build/aclocal-path:; echo export ACLOCAL_PATH:="`aclocal --print-ac-dir`":'$$(ACLOCAL_PATH)' >$@
usr-build/aclocal-path2:; echo export ACLOCAL_PATH:="`usr-build/bin/aclocal --print-ac-dir`":'$$(ACLOCAL_PATH)' >$@
get-m4:
	$(MAKE) -C usr-build -f Makefile.pkg LIBNAME=m4 VERSION=1.4.17
get-autoconf:get-m4
	$(MAKE) -C usr-build -f Makefile.pkg LIBNAME=autoconf VERSION=2.69
get-automake:get-autoconf
	$(MAKE) -C usr-build -f Makefile.pkg LIBNAME=automake VERSION=1.14.1
get-libtool:get-automake
	$(MAKE) -C usr-build -f Makefile.pkg LIBNAME=libtool VERSION=2.4.2
get-pkg: usr-build/share/aclocal/pkg.m4
usr-build/aclocal-path2:get-automake
usr-build/share/aclocal/pkg.m4: usr-build/pkg.m4
	if [ ! -d usr-build/share ] ; then mkdir usr-build/share ; fi
	if [ ! -d usr-build/share/aclocal ] ; then mkdir usr-build/share/aclocal ; fi
	cp $^ $@
clean-tools:; rm -rf usr-build/aclocal-path usr-build/aclocal-path2 usr-build/bin usr-build/include usr-build/lib usr-build/share usr-build/src usr-build/.compiled-* usr-build/.configured-* usr-build/.installed-*  usr-build/.patched-* usr-build/.untarred-*

autoconf-absent : autoconf-absent-msg autoconf-msg 
autoconf-absent-msg :
	@ echo "=============================================================================" >&2
	@ echo "Error: the autoconf program is not installed on your system." >&2

autoconf-old : autoconf-old-msg autoconf-msg 
autoconf-old-msg :
	@ echo "=============================================================================" >&2
	@ echo "error: the autoconf program installed on your system (`type autoconf`) is too old." >&2

autoconf-msg :
	@ echo "" >&2
	@ echo "Please install or update autoconf: the version (`autoconf --version | sed -e '1s/.*\([1-9][0-9]*\..*\)/\1/' -e '2,$$d'`) should be at least $(AUTOCONF_VERSION)." >&2
	@ echo "" >&2
	@ echo "Alternatively, type" >&2
	@ echo "" >&2
	@ echo "    make get-tools" >&2
	@ echo "" >&2
	@ echo "for recent versions of m4, autoconf, automake, and libtool to be downloaded and compiled automatically." >&2
	@ echo "=============================================================================" >&2
	@ false

check-autoconf :
	@ type autoconf >/dev/null || $(MAKE) -f Makefile autoconf-absent
	@ if test `autoconf --version | sed -e '1s/.* \([1-9][0-9]*\)\..*/\1/' -e '2,$$d'` -gt "$(AUTOCONF_MAJOR)" \
	  || \
	    test `autoconf --version | sed -e '1s/.* \([1-9][0-9]*\)\..*/\1/' -e '2,$$d'`  = "$(AUTOCONF_MAJOR)" \
	    && \
	    test `autoconf --version | sed -e '1s/.*\.\([1-9][0-9]*\).*/\1/' -e '2,$$d'`   -ge "$(AUTOCONF_MINOR)" ;\
	  then echo GNU autoconf version okay ;\
	  else $(MAKE) -f Makefile autoconf-old ;\
	  fi

automake-absent : automake-absent-msg automake-msg 
automake-absent-msg :
	@ echo "=============================================================================" >&2
	@ echo "Error: the automake program is not installed on your system." >&2

automake-old : automake-old-msg automake-msg 
automake-old-msg :
	@ echo "=============================================================================" >&2
	@ echo "error: the automake program installed on your system (`type automake`) is too old." >&2

automake-msg :
	@ echo "" >&2
	@ echo "Please install or update automake: the version (`automake --version | sed -e '1s/.* \(.*\)/\1/' -e '2,$$d'`) should be at least $(AUTOMAKE_VERSION)." >&2
	@ echo "" >&2
	@ echo "Alternatively, type" >&2
	@ echo "" >&2
	@ echo "    make get-tools" >&2
	@ echo "" >&2
	@ echo "for recent versions of m4, autoconf, automake, and libtool to be downloaded and compiled automatically." >&2
	@ echo "=============================================================================" >&2
	@ false

check-automake :
	@ type automake >/dev/null || $(MAKE) -f Makefile automake-absent
	@ if test `automake --version | sed -e '1s/.* \([1-9][0-9]*\)\..*/\1/' -e '2,$$d'` -gt "$(AUTOMAKE_MAJOR)" \
	  || \
	    test `automake --version | sed -e '1s/.* \([1-9][0-9]*\)\..*/\1/' -e '2,$$d'`  =  "$(AUTOMAKE_MAJOR)" \
	    && \
	    test `automake --version | sed -e '1s/.* [1-9][0-9]*\.\([1-9][0-9]*\).*/\1/' -e '2,$$d'`  -ge "$(AUTOMAKE_MINOR)" ;\
	  then echo GNU automake version okay ;\
	  else $(MAKE) -f Makefile automake-old ;\
	  fi

# autoheader's job is to make include/config.h, but if there are no changes, it doesn't touch it, 
# which means "make" will keep trying
include/M2/config.h.in.stamp : configure.ac # aclocal.m4
	autoheader
	touch "$@"

monitor:
	@ if [ ! -f include/config.h.in -a -f include/config.h.in.stamp ]; then set -x; rm include/config.h.in.stamp; fi

count count-source-code-lines:
	find . \( \
	-name BUILD -prune -false -o \
	-name .svn -prune -false -o \
	-name bugs -prune -false -o \
	-name submodules -prune -false -o \
	-name autoconf-\* -prune -false -o \
	-name regex -prune -false -o \
	-name examples -prune -false -o \
	-name test -prune -false -o \
	-name TST -prune -false -o \
	-name EXA -prune -false -o \
	-name ComputationsBook -prune -false -o \
	-name basictests -prune -false -o \
	-name \*.m2 -o \
	-name \*.c -o \
	-name \*.h -o \
	-name \*.hpp -o \
	-name \*.cpp -o \
	-name configure.ac -o \
	-name Makefile.in -o \
	-name Makefile -o \
	-name GNUMakefile \) \
	-not -name bug\* \
	-not -name demo\* \
	-not -name test\* \
	| xargs wc -l > /tmp/$@-$$$$ && \
	egrep -v ' total$$' /tmp/$@-$$$$ && \
	egrep ' total$$' /tmp/$@-$$$$ && \
	egrep ' total$$' /tmp/$@-$$$$ | awk '{sum += $$1}; END { print sum, "grand total" }' && \
	rm /tmp/$@-$$$$


SEARCH=
ifeq ($(SEARCH),)
grep :
	@echo 'usage: make grep SEARCH="regexp"' >&2
	@false
else
grep :
	find . \( -name .svn -o -name BUILD -o -name autom4te.cache \) ! -prune -o -type f | xargs grep -nH -e $(SEARCH) || [ $$? = 123 ]
endif

announce:
	@ echo '-*- compilation; coding: utf-8 -*-'
	@ echo ' -- Macaulay2 compilation:'
	@ echo ' --   Paste this output into an emacs buffer and run M-x compilation-mode'
	@ echo ' --   and then C-C ` (same as M-x next-error) to view the errors.'

TAGS.config : config/files always
	(echo configure.ac ; cat $< | sed 's/$$/.in/') | xargs etags -o $@
always:

diffs :;@ i=1 ; while test -f diffs-$$i ; do i=$$(expr $$i + 1 ) ; done ; svn diff >diffs-$$i ; echo created diffs-$$i

check-vardirs:
	egrep -nH '@(bin|data|include|info|lib|libexec|localstate|man|sbin|sharedstate|sysconf|ps|pdf|dvi|html|locale|doc|dataroot|packages|libm2|emacs|libraries|packagecachecore)dir@' $(shell cat ./config/files | sed 's/$$/.in/')
check-pre-vardirs:
	egrep -nH '@pre_(bin|data|include|info|lib|libexec|localstate|man|sbin|sharedstate|sysconf|ps|pdf|dvi|html|locale|doc|dataroot|packages|libm2|emacs|libraries|packagecachecore)dir@' $(shell cat ./config/files | sed 's/$$/.in/')

# Local Variables:
# mode: Makefile
# compile-command: "make -f Makefile "
# End:
