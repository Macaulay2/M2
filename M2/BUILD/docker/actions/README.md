# Testing the GitHub Actions Build (Ubuntu only)

The `Dockerfile` in this directory creates a container image based on Ubuntu 20.04 intended to mimic the [Ubuntu virtual environment](https://github.com/actions/virtual-environments/blob/ubuntu20/20201210.0/images/linux/Ubuntu2004-README.md) used by GitHub Actions. This container can be used for debugging issues with the actions.

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
