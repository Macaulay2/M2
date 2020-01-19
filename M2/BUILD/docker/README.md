# Running Macaulay2 in Docker

Docker is a software for building, shipping, or running applications on different
platforms. A Dockerfile contains instructions for building an image with Macaulay2.

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

## Accessing files and packages

A local folder named `storage` is shared as the `/home` directory inside the container.
Furthermore, the current Macaulay2 git repository is available at `/home/M2`.

## Other Use Cases
 - build an RPM package for an arbitrary version of Fedora Linux from any platform.
 - run the latest version of Macaulay2 on any OS that runs Docker (including Mac, Windows, etc.).
 - develop Macaulay2 by running it on different libraries or kernels simultaneously.
 - run any number of threads of the latest version of Macaulay2 in the cloud efficiently.
