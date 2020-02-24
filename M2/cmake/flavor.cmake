set(CMAKE_VERBOSE_MAKEFILE OFF)

# FIXME: This is hardcoded for now. Change it to your version here:

set(ISSUE_FLAVOR   "Fedora") # e.g. Fedora, Ubuntu
set(ISSUE_RELEASE  "31")     # e.g. 31, 7.10

#if test ! "$ISSUE"
#then if test -f /usr/bin/sw_vers
#     then ISSUE_FLAVOR=`/usr/bin/sw_vers -productName`
#          ISSUE_RELEASE=`/usr/bin/sw_vers -productVersion`
#     elif test -f /usr/bin/lsb_release
#     then ISSUE_FLAVOR=`lsb_release -s --id`
#          ISSUE_RELEASE=`lsb_release -s --release`
#     elif test -f /etc/os-release
#     then ISSUE_FLAVOR=`. /etc/os-release ; echo $ID`
#          ISSUE_RELEASE=`. /etc/os-release ; echo $VERSION_ID`
#     elif test -f /usr/lib/os-release
#     then ISSUE_FLAVOR=`. /usr/lib/os-release ; echo $ID`
#          ISSUE_RELEASE=`. /usr/lib/os-release ; echo $VERSION_ID`
#     elif test -f /bin/freebsd-version
#     then ISSUE_FLAVOR=FreeBSD
#          ISSUE_RELEASE=`freebsd-version`
#     elif test -f /etc/system-release
#     then ISSUE_FLAVOR=[`</etc/system-release head -1 | sed 's/^\([A-Za-z ]*\).*/\1/' | sed 's/ //g' `]
#          ISSUE_RELEASE=[`</etc/system-release head -1 | sed 's/[^0-9]*\([0-9.]*\).*/\1/'`]
#     elif test -f /etc/issue
#     then ISSUE_FLAVOR=[`</etc/issue head -1 | sed 's/^\([A-Za-z ]*\).*/\1/' | sed 's/ //g' `]
#          ISSUE_RELEASE=[`</etc/issue head -1 | sed 's/[^0-9]*\([0-9.]*\).*/\1/'`]
#     fi
#     # translate to something standard (for us), and verify :
#     case $ISSUE_FLAVOR in
#         "Mac OS X") ISSUE_FLAVOR=MacOS ;;
#	 "debian"|"Debian"*) ISSUE_FLAVOR=Debian ;;
#         "ubuntu"|"Ubuntu") ISSUE_FLAVOR=Ubuntu ;;
#	 "FedoraCore"*) ISSUE_FLAVOR=FedoraCore ;;
#	 "Fedora"*) ISSUE_FLAVOR=Fedora ;;
#	 "RedHatEnterprise"*) ISSUE_FLAVOR=RedHatEnterprise ;;
#	 "RedHat"*) ISSUE_FLAVOR=RedHat ;;
#	 "Scientific"*) ISSUE_FLAVOR="ScientificLinux" ;;
#	 "Raspbian"*) ISSUE_FLAVOR=Raspbian ;;
#	 *"openSUSE") ISSUE_FLAVOR=openSUSE ;;
#	 "SUSE LINUX") ISSUE_FLAVOR=SuseLinux ;;
#	 "arch") ISSUE_FLAVOR=ArchLinux ; ISSUE_RELEASE=none ;;
#	 "") AC_MSG_ERROR([issue not found]) ;;
#	 *)  AC_MSG_NOTICE([unrecognized issue: $ISSUE_FLAVOR]) ;;
#     esac
#     ISSUE_FLAVOR=`echo $ISSUE_FLAVOR | sed 's/ /-/g'`
#     case $ISSUE_RELEASE in
#         none) ISSUE=$ISSUE_FLAVOR ;;
#	 "")   AC_MSG_NOTICE([release number not found])
#	       ISSUE_RELEASE=unknown
#	       ISSUE=$ISSUE_FLAVOR-unknown ;;
#	 *)    ISSUE=$ISSUE_FLAVOR-$ISSUE_RELEASE
#     esac
#
#fi

## some operating systems have no ISSUE_FLAVOR, e.g., MacOS
#test "$ISSUE" || ISSUE=$REL
