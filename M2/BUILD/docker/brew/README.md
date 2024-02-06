# Bottling Macaulay2 for [Homebrew](https://brew.sh/)

Homebrew is a package manager for macOS and Linux. The `Dockerfile` in this directory creates a container image based on Ubuntu 20.04 for building and testing the Homebrew formula for Macaulay2 available at [`Macaulay2/tap`](https://github.com/Macaulay2/homebrew-tap).

## Getting Started
0. Install Docker and start the daemon (optionally, also install [Homebrew](https://brew.sh/)).

1. Build the container using Docker:
```
make build
```

2. Enter the shell:
```
make shell-linux # if you are using Linuxbrew
make shell-macos # if you are using Homebrew
```

3. Bottle Macaulay2:
```
brew install --keep-tmp --verbose --build-bottle Macaulay2/tap/M2
```
