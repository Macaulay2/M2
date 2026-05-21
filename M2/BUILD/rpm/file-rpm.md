# BUILD/rpm — RPM packaging in a Docker container

The `rpm/` subdirectory produces **Fedora/RHEL-compatible `.rpm`
packages** by running `rpmbuild` inside a Docker container.

Part of [`BUILD/`](../README.md).

[← BUILD/ overview](../README.md) · [← BUILD/rpm README](README.md) · [← top-level repo TOC](../../../README.md)

## Files

- **`Makefile`** — orchestrates the container build + rpmbuild.
- **`Macaulay2.spec`** — RPM spec file.
- **`README.md`** — usage notes.

## `Macaulay2.spec` — the RPM spec

```spec
# very loosely based on the fedora .spec:
# https://src.fedoraproject.org/rpms/Macaulay2/blob/rawhide/f/Macaulay2.spec

Name:    Macaulay2
Version: 1.26.05

# release convention: 0.x.m2 (so official fedora package will take precedence)
# increment x as needed, reset to 1 with each new m2 release
Release: 0.1.m2%{?dist}
Summary: System for algebraic geometry and commutative algebra

# https://github.com/Macaulay2/M2/issues/2604
%global _lto_cflags %{nil}

License: GPL-2.0-or-later
URL:     https://macaulay2.com/
```

Key spec conventions:

- **`0.x.m2` release naming** — the `0.` prefix ensures Fedora's
  official package (which uses higher release numbers) is
  preferred when both are installed.
- **`%global _lto_cflags %{nil}`** — disables LTO. Issue
  #2604 documents why LTO breaks M2 in some configurations.
- **License** — `GPL-2.0-or-later`. M2's effective license under
  Fedora's SPDX conventions.

The spec then declares `BuildRequires:` for every build dependency
(autoconf, bison, flex, gcc-c++, gcc-gfortran, ...).

## `Makefile` — Docker orchestration

```makefile
DISTRIBUTION = almalinux
RELEASE = latest
VERSION := $(shell cat ../../VERSION)
TARNAME = Macaulay2-$(VERSION)

ifeq ($(DISTRIBUTION), fedora)
	DOCKERDIR = fedora
else
	DOCKERDIR = rhel
endif

HOST_RPMBUILD = rpmbuild-$(DISTRIBUTION)-$(RELEASE)
CTR_RPMBUILD = /home/macaulay/rpmbuild

rpm: $(HOST_RPMBUILD) .image-built-$(DISTRIBUTION)-$(RELEASE)
	sudo chown -R 1000:0 $(HOST_RPMBUILD)
	docker run \
		--entrypoint bash                             \
		-v $(CURDIR)/$(HOST_RPMBUILD):$(CTR_RPMBUILD) \
		m2-$(DISTRIBUTION)-$(RELEASE)-build           \
		-c 'set -e; \
			rpmbuild -ba $(CTR_RPMBUILD)/SPECS/Macaulay2.spec'
```

The build process:

1. Read `M2/VERSION` for the package version.
2. Pick distribution (`fedora` or default `almalinux`).
3. Find/build a Docker image with the RPM build tools.
4. Set up a `rpmbuild-<dist>-<rel>/` tree on the host.
5. Copy `Macaulay2.spec` to `SPECS/`.
6. Run `docker run rpmbuild -ba SPECS/Macaulay2.spec`.
7. Output `.rpm` files in `<host_rpmbuild>/RPMS/x86_64/`.

## Why a separate stream from distributions/

The `distributions/` directory has its own RPM machinery, but:

- That one uses autotools-only patterns and runs on the host.
- This one uses Docker, isolates the host environment, and targets
  multiple RPM-based distros.

Two streams, two trade-offs. The Docker stream is the
**recommended path** for release artifact production.

## Variations

By default uses AlmaLinux. To target Fedora or RHEL specifically:

```sh
make rpm DISTRIBUTION=fedora
make rpm DISTRIBUTION=rhel
```

## Used by

- Release managers cutting Fedora/RHEL packages.
- Developers checking RPM-build compatibility.
- CI's release-artifact production.

## Related

- [`../file-build-layout.md`](../file-build-layout.md) — BUILD/
  overview.
- [`../docker/file-docker.md`](../docker/file-docker.md) — sister
  Docker-based builds.
- [`../../distributions/file-distributions.md`](../../distributions/file-distributions.md)
  — autotools-side packaging.
