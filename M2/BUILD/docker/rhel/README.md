# Compiling Macaulay2 in a Red Hat Enterprise Linux-compatible Container

The `Dockerfile` in this directory creates a container image based a Red Hat Enterprise Linux-compatible distribution (either Rocky Linux or AlmaLInux)  with all the required libraries and software for compiling Macaulay2.

## Getting Started
0. Install Docker and start the daemon.

1. Compile Macaulay2 in a container:
```
make build
```

After the build is complete, rpm packages will be available in `M2/BUILD/build-docker`.

2. Run Macaulay2:
```
make run
```

By default, the above targets are run on Rocky Linux 9.  To switch to AlmaLinux, add `DISTRIBUTION=almalinux`.  To switch to an RHEL 8-compatible release, add `RELEASE=8`.
