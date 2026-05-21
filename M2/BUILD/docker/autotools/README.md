# `M2/BUILD/docker/autotools/` — autotools build helper for Docker

A reusable Makefile fragment that adds a `build-autotools` target to any
of the platform-specific docker recipes ([`debian/`](../debian/README.md),
[`fedora/`](../fedora/README.md), [`rhel/`](../rhel/README.md), …). Including
it from another Makefile lets that recipe drive an autotools build inside its
container with one command.

| File | Role |
|---|---|
| `Makefile` | Defines the `M2_BUILD_SCRIPT_autotools` shell snippet and the `build-autotools` Make target |

## Usage from another docker recipe

```make
include ../autotools/Makefile
```

That gives you `make build-autotools`, which runs `autogen.sh`, `configure`,
and `make` inside the container.

## Related

- [`../`](../README.md) — Docker build overview.
- [`../../README.md`](../../README.md) — build-tree conventions.
