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

This can also be automated on GitHub using the "Build RPM packages" workflow.

# Publishing Macaulay2 rpm packages

*(For M2 core developers with login access to aeschylus.)*

After building the rpm packages, upload each of them to the appropriate
directory:

* Fedora: `/var/www/www2.macaulay2.com/Repositories/Fedora/${RELEASE}`
* RHEL: `/var/www/www2.macaulay2.com/Repositories/Scientific/${RELEASE}`

Then in that directory, run:

```
createrepo .
```

After doing this for each package, run `make` in `/var/www/www2.macaulay2.com/`
to regenerate all the html pages.
