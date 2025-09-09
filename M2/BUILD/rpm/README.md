# Building Macaulay2 rpm packages

This directory contains scripts for building Macaulay2 rpm packages for Fedora,
Red Hat Enterprise Linux, and RHEL-compatible distributions like Rocky Linux
and AlmaLinux.  The packages are built in Docker containers.  To get started,
run:

```
make rpm
```

By default, this will build a package for the latest version of AlmaLinux.  The
distribution can be controlled using the `DISTRIBUTION` variable and the release
the `RELEASE` variable, e.g.,

```
make rpm DISTRIBUTION=fedora RELEASE=42
```

The build artifacts are deposited in the `rpmbuild-$(DISTRIBUTION)-$(RELEASE)`
directory, and in particular, the rpm package file may be found in the `RPMS`
subdirectory of that directory.
