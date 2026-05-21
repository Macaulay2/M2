# BUILD/docker — Docker-based build & packaging

The `docker/` subdirectory contains **Docker recipes** for
building, packaging, and testing M2 in clean containers. Useful
for CI, distribution maintenance, and reproducibility.

Part of [`BUILD/`](../README.md).

[← BUILD/ overview](../README.md) · [← BUILD/docker README](README.md) · [← top-level repo TOC](../../../README.md)

## Top-level Dockerfile — PPA install demo

```dockerfile
# Time usage: <5min
# Net usage:  ~200MB
# Disk usage: <800MB docker image

FROM ubuntu:latest

# Setting up Macaulay2 repository
ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get update && \
    apt-get install -y --no-install-recommends software-properties-common apt-transport-https && \
    add-apt-repository ppa:macaulay2/macaulay2 && apt-get update && apt-get clean

# Install Macaulay2
RUN apt-get install -y --no-install-recommends macaulay2 && apt-get clean
```

The header tells you the resource cost up-front: build takes
under 5 minutes and the resulting image is under 800MB.

This Dockerfile is the **end-user demo** — installs M2 from the
official Ubuntu PPA. Use it when you want a one-line "try M2 in a
clean environment" command:

```sh
docker build -t m2 .
docker run -it m2 M2
```

Plus a few QoL additions: emacs, M2-emacs (`elpa-macaulay2`),
bash completion, curl/git/mlocate.

## Subdirectory recipes

| Subdir | Purpose |
|---|---|
| `ubuntu/` | Compile from source in a clean Ubuntu container |
| `debian/` | Produce `.deb` packages |
| `fedora/` | Produce `.rpm` packages (Fedora) |
| `rhel/` | Produce `.rpm` packages (Red Hat Enterprise) |
| `arch/` | Arch Linux build |
| `autotools/` | Autotools-only build (no CMake) |
| `brew/` | Homebrew bottle production |
| `testbot/` | GitHub Actions test runner |
| `nightly/` | Nightly-build smoke tests |
| `actions/` | CI action validation |

Each subdir contains:

- **`Dockerfile`** — base image + M2-specific build steps.
- **`Makefile`** — convenience wrapper (build / run / clean).
- **`README.md`** — usage notes.

## The Makefile pattern

```makefile
TAG = m2
M2_HOME = /home/macaulay
M2_REPO = $(shell git rev-parse --show-toplevel)
STORAGE = $(M2_REPO)/M2/BUILD/docker/storage
VOLUME = --volume $(STORAGE):$(M2_HOME) --volume $(M2_REPO):$(M2_HOME)/M2
BUILD_DIR = M2/BUILD/build-docker

all: build run
```

Three things to note:

1. **`STORAGE`** — persistent storage *outside* the container so
   ccache, downloaded tarballs, etc. survive container restarts.
2. **`VOLUME`** — mounts the M2 source tree from the host into the
   container. Changes to source files are visible immediately;
   builds happen against live source.
3. **`BUILD_DIR`** — a separate build directory so the in-container
   build doesn't pollute the host's `M2/BUILD/build`.

## "Some may be outdated" caveat

The README explicitly notes:

> **Note**: some of the above may be outdated and require slight
> modifications before use.

Docker base images and OS conventions drift. The Dockerfiles are
maintained best-effort; if you use one and find it broken,
patches welcome.

## Used by

- CI workflows (`testbot/`, `actions/`).
- Distribution maintainers (`debian/`, `fedora/`, etc.).
- Reproducibility-conscious developers.

## Related

- [`../file-build-layout.md`](../file-build-layout.md) — BUILD/
  overview.
- [`../../distributions/README.md`](../../distributions/README.md)
  — autotools-side packaging.
- [`../rpm/file-rpm.md`](../rpm/file-rpm.md) — separate RPM
  packaging stream.
