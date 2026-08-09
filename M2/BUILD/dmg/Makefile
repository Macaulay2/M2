# Macaulay2 macOS DMG builder
#
# Targets:
#   all   -- configure, build, install, bundle deps, create DMG
#   check -- mount and test the DMG
#
# Variables (set via environment or make command line):
#   WORKSPACE         -- repository root (staging/ lives here; default: three levels up)
#   ARCH              -- arm64 or x86_64 (default: current machine architecture)
#   DEPLOYMENT_TARGET -- minimum macOS version (default: 14.0)
#   RUNNER_TEMP       -- scratch directory for DMG staging and testing (default: /tmp)
#   DMG_REVISION      -- revision of the DMG for a given M2 version (default: 1)
#   FACTORY_PREFIX    -- where factory is installed (default: brew --prefix factory)
#   GFTABLES_SRC      -- gftables directory to bundle (default: $(FACTORY_PREFIX)/share/factory/gftables)
#   GFTABLES_REL      -- where they go inside the prefix (default: share/Macaulay2/Core/factory)

SHELL       := /bin/bash
.SHELLFLAGS := -e -o pipefail -c
# .ONESHELL is required: the bundle target defines bash functions that are
# called later in the same recipe.
#
# GNU make 3.82 or later is required, for both .ONESHELL and .SHELLFLAGS.
# macOS ships 3.81 as /usr/bin/make and it ignores both silently -- recipes
# then run one shell per line with neither -e nor pipefail, which breaks
# multi-line shell constructs in confusing ways.  Every workflow job running
# this makefile installs make from Homebrew for that reason.
.ONESHELL:

SRCDIR      := $(abspath ../..)
BUILDDIR    := $(abspath ../build)

WORKSPACE         ?= $(abspath ../../..)
ARCH              ?= $(shell uname -m)
DEPLOYMENT_TARGET ?= 14.0
RUNNER_TEMP       ?= /tmp
# Bump when rebuilding the DMG for an M2 version that has already been
# released; it appears in the DMG filename only, not in the volume name or the
# installed directory.
DMG_REVISION      ?= 1

M2_VERSION   := $(shell cat $(SRCDIR)/VERSION)
PKG_NAME     := Macaulay2-$(M2_VERSION)
PKG          := $(WORKSPACE)/staging/$(PKG_NAME)
DMG          := $(WORKSPACE)/$(PKG_NAME)-$(DMG_REVISION)-$(ARCH)-macOS.dmg
LIB_DIR      := $(PKG)/lib/Macaulay2/lib
PROGRAMS_DIR := $(PKG)/libexec/Macaulay2/bin
BREW         := $(shell brew --prefix)
NCPU         := $(shell sysctl -n hw.logicalcpu)

# Factory's finite field addition tables.  Homebrew's factory keeps them
# outside our prefix, so we stage a copy inside the DMG and tell configure to
# record a ${prefix}-relative path; otherwise the absolute Homebrew path gets
# compiled into M2-binary and M2 fails to start on machines without Homebrew.
# See https://github.com/Macaulay2/M2/issues/4596.
FACTORY_PREFIX   ?= $(shell brew --prefix factory 2>/dev/null)
GFTABLES_SRC     ?= $(FACTORY_PREFIX)/share/factory/gftables
GFTABLES_REL     ?= share/Macaulay2/Core/factory
GFTABLES_STAGING := $(BUILDDIR)/usr-dist/common/$(GFTABLES_REL)
# What configure records in the layout, e.g. ${datadir}/Macaulay2/Core/factory/.
# It has to stay under $(datadir) and be written with an unexpanded ${datadir}
# so that configure normalizes it to a ${prefix}-relative path.
GFTABLESDIR      := $${datadir}/$(patsubst share/%,%,$(GFTABLES_REL))/

F77          := gfortran-14
FC           := gfortran-14
DMG_STAGING      := $(RUNNER_TEMP)/dmg-root
MOUNT_DIR        := $(RUNNER_TEMP)/M2-dmg
INSTALL_DIR      := $(RUNNER_TEMP)/M2-installed
INSTALLED_STAMP  := $(INSTALL_DIR)/.installed

.PHONY: all gftables configure build install programs bundle dmg install-dmg run-basic-tests run-core-tests run-package-tests check

all: dmg

# ----------------------
#   Stage Factory's gftables
# ----------------------

# The staging area is where the M2 that runs during the build looks for the
# tables, and "make install" ships only what has been staged.

gftables:
	test -d "$(GFTABLES_SRC)" || \
	    { echo "error: gftables not found at $(GFTABLES_SRC)" >&2; exit 1; }
	mkdir -p $(GFTABLES_STAGING)
	cp -RL $(GFTABLES_SRC) $(GFTABLES_STAGING)/

# ----------------------
#   Configure Macaulay2
# ----------------------

configure:
	mkdir -p $(BUILDDIR)
	cd $(BUILDDIR) && $(SRCDIR)/autogen.sh
	cd $(BUILDDIR) && F77=$(F77) FC=$(FC) MACOSX_DEPLOYMENT_TARGET=$(DEPLOYMENT_TARGET) \
	    $(SRCDIR)/configure \
	    --prefix=$(PKG) \
	    GFTABLESDIR='$(GFTABLESDIR)' \
	    --with-system-libs --with-fplll --without-python

# ----------------------
#   Build Macaulay2
# ----------------------

build: configure gftables
	$(MAKE) -C $(BUILDDIR) -j$(NCPU)

# ----------------------
#   Install Macaulay2
# ----------------------

install: build
	$(MAKE) -C $(BUILDDIR) install

# ----------------------
#   Copy external programs from Homebrew
# ----------------------

programs: install
	mkdir -p $(PROGRAMS_DIR)
	for formula in 4ti2 cohomcalg csdp gfan lrs msolve nauty normaliz topcom; do
	    prefix=$$(brew --prefix "$$formula" 2>/dev/null) || continue
	    for bin in "$$prefix/bin/"*; do
	        dst="$(PROGRAMS_DIR)/$$(basename "$$bin")"
	        if [ -f "$$bin" ] && [ ! -f "$$dst" ]; then
	            cp "$$bin" "$$dst"
	        fi
	    done
	done
	# phcpack and bertini are not on Homebrew; they are pre-staged by the
	# workflow into RUNNER_TEMP.  Include each only if it was staged.
	#
	# These two go in bin/ rather than alongside the programs above, because
	# PHCpack.m2 and Bertini.m2 look up their program with the shell rather
	# than findProgram and so need it on the user's PATH.  bin/ is the
	# directory the DMG tells users to add; $(PROGRAMS_DIR) is only ever
	# searched by findProgram.
	for prog in phc bertini; do
	    src="$(RUNNER_TEMP)/$$prog"
	    dst="$(PKG)/bin/$$prog"
	    if [ -f "$$src" ] && [ ! -f "$$dst" ]; then
	        cp "$$src" "$$dst"
	    fi
	done

# ----------------------
#   Bundle non-system dylibs
# ----------------------

bundle: programs
	mkdir -p $(LIB_DIR)

	# Pre-copy non-system Homebrew deps that are referenced via @rpath/,
	# @executable_path/, or absolute paths so dylibbundler can find them.
	prefill_from_homebrew() {
	    otool -L "$$1" 2>/dev/null | tail -n +2 | awk '{print $$1}' | while read -r ref; do
	        depname=$$(basename "$$ref")
	        if [ -f "$(LIB_DIR)/$$depname" ]; then continue; fi
	        case "$$ref" in
	            /usr/lib/*|/System/*) continue ;;
	            "$(PKG)"*) continue ;;
	            @executable_path/*|@loader_path/*|@rpath/*)
	                src=$$(ls "$(BREW)/opt/"*"/lib/$$depname" "$(BREW)/lib/$$depname" 2>/dev/null | head -1 || true)
	                ;;
	            /*)
	                [ -f "$$ref" ] && src="$$ref" || continue
	                ;;
	            *) continue ;;
	        esac
	        [ -n "$$src" ] || continue
	        cp "$$src" "$(LIB_DIR)/$$depname"
	        chmod +w "$(LIB_DIR)/$$depname"
	        echo "Pre-copied: $$depname"
	    done
	}

	# Pass 1: scan executables and installed dylibs.
	for exe in "$(PKG)/bin/"* "$(PKG)/libexec/Macaulay2/bin/"*; do
	    [ -f "$$exe" ] && file "$$exe" | grep -q "Mach-O" || continue
	    prefill_from_homebrew "$$exe"
	done
	find "$(PKG)/lib" -name "*.dylib" -not -type l 2>/dev/null | while read -r dylib; do
	    prefill_from_homebrew "$$dylib"
	done

	# Pass 2: transitive deps of the libs we just copied.
	for dylib in "$(LIB_DIR)/"*; do
	    [ -f "$$dylib" ] && file "$$dylib" | grep -q "Mach-O" || continue
	    prefill_from_homebrew "$$dylib"
	done

	# Run dylibbundler, then strip all LC_RPATH entries and replace with
	# exactly one pointing to lib/Macaulay2/lib/.
	bundle_exe() {
	    local exe="$$1" rpath="$$2"
	    file "$$exe" | grep -q "Mach-O" || return 0
	    dylibbundler -of -b -x "$$exe" -d "$(LIB_DIR)" -p "@rpath/" \
	        -s "$(BREW)/lib" -s "$$(brew --prefix libomp)/lib" \
	        -s "$$(brew --prefix readline)/lib" -s "$(LIB_DIR)" \
	        --overwrite-files
	    while IFS= read -r rp; do
	        install_name_tool -delete_rpath "$$rp" "$$exe" 2>/dev/null || true
	    done < <(otool -l "$$exe" | awk '/LC_RPATH/{found=1} found && /path /{print $$2; found=0}')
	    install_name_tool -add_rpath "$$rpath" "$$exe"
	}

	echo "=== lib/Macaulay2/lib/ after prefill, before dylibbundler ===" && ls -la "$(LIB_DIR)/" || echo "(empty)"

	# bin/ executables: @executable_path/../lib/Macaulay2/lib/
	for exe in "$(PKG)/bin/"*; do
	    [ -f "$$exe" ] && [ -x "$$exe" ] && [ ! -L "$$exe" ] || continue
	    bundle_exe "$$exe" @executable_path/../lib/Macaulay2/lib/
	done

	# libexec/Macaulay2/bin/ executables: three levels up to lib/Macaulay2/lib/
	for exe in "$(PKG)/libexec/Macaulay2/bin/"*; do
	    [ -f "$$exe" ] && [ -x "$$exe" ] && [ ! -L "$$exe" ] || continue
	    bundle_exe "$$exe" @executable_path/../../../lib/Macaulay2/lib/
	done

	# Rewrite @rpath/libXXX load commands in bin/ executables to direct
	# @executable_path/... paths (they have -headerpad_max_install_names).
	for exe in "$(PKG)/bin/"*; do
	    [ -f "$$exe" ] && [ -x "$$exe" ] && [ ! -L "$$exe" ] || continue
	    file "$$exe" | grep -q "Mach-O" || continue
	    otool -L "$$exe" 2>/dev/null | tail -n +2 | awk '{print $$1}' | while read -r dep; do
	        case "$$dep" in @rpath/*)
	            depname=$$(basename "$$dep")
	            [ -f "$(LIB_DIR)/$$depname" ] || continue
	            want="@executable_path/../lib/Macaulay2/lib/$$depname"
	            install_name_tool -change "$$dep" "$$want" "$$exe" 2>/dev/null || true
	            echo "  fix_rpath: $$(basename "$$exe"): $$dep -> $$want"
	            ;;
	        esac
	    done
	done

	# Fix dylib-to-dylib references; strip hardened-runtime signatures first
	# so install_name_tool can modify them, then remove @rpath/ LC_RPATH entries
	# that would cause a circular dyld reference.
	for dylib in "$(LIB_DIR)/"*; do
	    [ -f "$$dylib" ] && file "$$dylib" | grep -q "Mach-O" || continue
	    libname=$$(basename "$$dylib")
	    codesign --remove-signature "$$dylib" 2>/dev/null || true
	    install_name_tool -id "@loader_path/$$libname" "$$dylib" 2>/dev/null || true
	    install_name_tool -delete_rpath "@rpath/" "$$dylib" 2>/dev/null || true
	    install_name_tool -delete_rpath "@rpath"  "$$dylib" 2>/dev/null || true
	    otool -L "$$dylib" 2>/dev/null | tail -n +2 | awk '{print $$1}' | while read -r dep; do
	        depname=$$(basename "$$dep")
	        [ -f "$(LIB_DIR)/$$depname" ] || continue
	        if [ "$$dep" = "@loader_path/$$depname" ]; then continue; fi
	        install_name_tool -change "$$dep" "@loader_path/$$depname" "$$dylib" 2>/dev/null || true
	    done
	done

	# Re-sign everything with an ad-hoc signature.
	for f in "$(LIB_DIR)/"* "$(PKG)/bin/"* "$(PKG)/libexec/Macaulay2/bin/"*; do
	    [ -f "$$f" ] && file "$$f" | grep -q "Mach-O" || continue
	    codesign --force --sign - "$$f" 2>/dev/null || true
	done

	echo "=== lib/Macaulay2/lib/ after bundling ===" && ls -la "$(LIB_DIR)/"
	echo "=== M2-binary rpaths ===" && otool -l "$(PKG)/bin/M2-binary" | grep -A2 LC_RPATH
	echo "=== M2-binary load commands ===" && otool -L "$(PKG)/bin/M2-binary"

# ----------------------
#   Package into a DMG
# ----------------------

dmg: bundle
	mkdir -p $(DMG_STAGING)
	cp -r $(PKG) $(DMG_STAGING)/
	ln -sf /Applications $(DMG_STAGING)/Applications
	hdiutil create \
	    -volname $(PKG_NAME) \
	    -srcfolder $(DMG_STAGING) \
	    -ov -format UDZO \
	    $(DMG)

# ----------------------
#   Install the DMG for testing
# ----------------------

install-dmg: $(INSTALLED_STAMP)

$(INSTALLED_STAMP):
	mkdir -p $(MOUNT_DIR) $(INSTALL_DIR)
	hdiutil attach $(DMG) -nobrowse -readonly -mountpoint $(MOUNT_DIR)
	ditto $(MOUNT_DIR)/$(PKG_NAME) $(INSTALL_DIR)
	hdiutil detach $(MOUNT_DIR)
	echo "=== $(INSTALL_DIR)/lib/Macaulay2/lib/ after ditto ===" && \
	    ls -la $(INSTALL_DIR)/lib/Macaulay2/lib/ || echo "(missing)"
	echo "=== M2-binary rpaths in install ===" && \
	    otool -l $(INSTALL_DIR)/bin/M2-binary | grep -A2 LC_RPATH
	echo "=== gftables in install ===" && \
	    grep -n '"factory gftables"' $(INSTALL_DIR)/share/Macaulay2/Core/startup.m2
	test -f $(INSTALL_DIR)/$(GFTABLES_REL)/gftables/961 || \
	    { echo "error: gftables missing from the DMG" >&2; exit 1; }
	if grep -Eq '"factory gftables" *=> *"/' $(INSTALL_DIR)/share/Macaulay2/Core/startup.m2; then
	    echo "error: an absolute gftables path was baked into the DMG" >&2
	    exit 1
	fi
	touch $(INSTALLED_STAMP)

# ----------------------
#   Test the DMG
# ----------------------

run-basic-tests: $(INSTALLED_STAMP)
	$(INSTALL_DIR)/bin/M2 -q --check 1

run-core-tests: $(INSTALLED_STAMP)
	$(INSTALL_DIR)/bin/M2 -q --check 2

run-package-tests: $(INSTALLED_STAMP)
	$(INSTALL_DIR)/bin/M2 -q --check 3

check: run-basic-tests run-core-tests run-package-tests
