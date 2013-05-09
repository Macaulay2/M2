# -*- makefile-gmake -*-

.PHONY : all monitor

VERBOSE = yes

all: check-make check-autoconf configure monitor include/M2/config.h.in.stamp helpful
helpful:
ifeq ($(VERBOSE),yes)
	@echo '--: The configure script in this directory has been prepared.'
	@echo '--: To configure and build Macaulay2:'
	@echo '--: '
	@echo '     mkdir BUILD/normal'
	@echo '     cd BUILD/normal'
	@echo '     ../../configure --prefix=/usr/local'
	@echo '     make install'
	@echo '--: '
	@echo '--: To reconfigure and try again:'
	@echo '--: '
	@echo '     cd BUILD/normal'
	@echo '     make reconfigure install'
	@echo '--: '
	@echo '--: To see all the options to "configure":'
	@echo '--: '
	@echo '     ./configure --help'
	@echo '--: '
endif

MAJOR = 2
MINOR = 62
# version 2.60 was the first one with the macro AC_PROG_MKDIR_P
# version 2.62 is required by the configure.ac in gc/libatomic_ops
AUTOCONF_VERSION = $(MAJOR).$(MINOR)

PATH := autoconf/final/bin:$(PATH)
export PATH

check-make:; @ echo "$(.FEATURES)" | grep order-only >/dev/null || (echo error: version >= 3.81 of GNU make is required >&2; false)
configure : configure.ac config/files config/ax_prog_cc_for_build.m4 # aclocal.m4
	@ set -x ; autoconf

get-autoconf :
	$(MAKE) -f Makefile -C autoconf
	$(MAKE) -f Makefile

rm-autoconf :; $(MAKE) -f Makefile -C autoconf clean

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
	@ echo "Please install or update autoconf: the version (`autoconf -V | sed -e '1s/.*\([1-9][0-9]*\..*\)/\1/' -e '2,$$d'`) should be at least $(AUTOCONF_VERSION)." >&2
	@ echo "" >&2
	@ echo "Alternatively, type" >&2
	@ echo "" >&2
	@ echo "    make get-autoconf" >&2
	@ echo "" >&2
	@ echo "for a recent version to be downloaded and compiled automatically." >&2
	@ echo "=============================================================================" >&2
	@ false

check-autoconf :
	@ type autoconf >/dev/null || $(MAKE) -f Makefile autoconf-absent
	@ test `autoconf -V | sed -e '1s/.* \([1-9][0-9]*\)\..*/\1/' -e '2,$$d'` -gt $(MAJOR) \
	  || \
	    test `autoconf -V | sed -e '1s/.* \([1-9][0-9]*\)\..*/\1/' -e '2,$$d'`  =  $(MAJOR) \
	    && \
	    test `autoconf -V | sed -e '1s/.*\.\([1-9][0-9]*\).*/\1/' -e '2,$$d'`   -ge $(MINOR) || $(MAKE) -f Makefile autoconf-old

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
