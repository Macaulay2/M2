# debian 64:
# apt-get install openssh-server subversion 
SSHPORT = 2227
RDPPORT = 3391
VERSION = 5.0.6
VERSION2 := $(shell echo $(VERSION) | sed 's=\.==g')
# ARCH = i386
ARCH = amd64
ISO = debian-$(VERSION2)-$(ARCH)-netinst.iso
URL = http://cdimage.debian.org/debian-cd/$(VERSION)/$(ARCH)/iso-cd/$(ISO)
include ../Makefile.machine
