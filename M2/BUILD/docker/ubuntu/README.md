# Compiling Macaulay2 in a Container

CMake is a cross-platform family of programs for building, testing, and packaging software.

The `Dockerfile` in this directory creates a container image based on Ubuntu 18.04.4 with all the required libraries and software for compiling Macaulay2.

## Getting Started
0. Install Docker and start the daemon.

1. Compile Macaulay2 in a container:
```
make build
```

2. Run Macaulay2:
```
make run
```
