# Building Macaulay2 using CMake in a Docker Container

Docker is a software for building, shipping, or running applications on different
platforms. A Dockerfile contains instructions for building an image with Macaulay2.

CMake is a cross-platform family of tools for building, testing and packaging software.

The `build-image` target in this directory creates a Docker image based on Ubuntu 18.04
with all the required libraries and software for compiling Macaulay2; the `build-M2`
target begins the compilation; and the `run` target starts M2 within Emacs.

## Getting Started
0. Install Docker and start the daemon.

1. Build a Macaulay2-in-a-container:
```
make build
```

2. Run Macaulay2-in-a-container:
```
make run
```

Note: if the graphical interface is not working, use the `run-terminal` target instead.

## Accessing files and packages

A local folder named `storage` is shared as the `/home/macaulay` directory inside the container.
Furthermore, the current Macaulay2 git repository is available at `/home/macaulay/M2`.

## Other Use Cases
 - build an RPM package for an arbitrary version of Fedora Linux from any platform.
 - run the latest version of Macaulay2 on any OS that runs Docker (including Mac, Windows, etc.).
 - develop Macaulay2 by running it on different libraries or kernels simultaneously.
 - run any number of threads of the latest version of Macaulay2 in the cloud efficiently.
