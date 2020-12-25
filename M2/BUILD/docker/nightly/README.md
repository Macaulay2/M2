# Testing the Nightly Build of Macaulay2

Every night, the latest version of Macaulay2 is built using a number of platforms and compilers using [Github Actions](https://github.com/Macaulay2/M2/actions). One of the builds, titled `cmake-ubuntu-latest-clang10`, uploads a packaged version of Macaulay2 for Ubuntu as a build artifact.

The `Dockerfile` in this directory creates a container image based on Ubuntu 18.04.4 for testing the nightly package built by Github Actions.

## Getting Started
0. Install Docker and start the daemon.

1. Find the latest [Build and Test Macaulay2](https://github.com/Macaulay2/M2/actions?query=workflow%3A%22Build+and+Test+Macaulay2%22) build

2. Download the artifact titled `Macaulay2-[commit info]-ubuntu-x86_64` and place it in this directory

3. Build the container using Docker:
```
make build
```

4. Check Macaulay2:
```
make check
make check-1 # runs basic tests
make check-2 # runs Core tests
make check-3 # runs all package tests
```
