# `Doxyfile.in` — templated Doxygen configuration

`Doxyfile.in` is the **templated Doxygen configuration**. The
CMake build substitutes paths and options into it at configure
time to produce the actual `Doxyfile` consumed by `doxygen` at
build time.

Part of [`docs/`](README.md).

[← docs/ overview](README.md) · [← top-level engine TOC](../../../README.md)

## Header

```text
# Doxyfile 1.8.15

# This file describes the settings to be used by the documentation system
# doxygen (www.doxygen.org) for a project.
#
# All text after a double hash (##) is considered a comment and is placed in
# front of the TAG it is preceding.
#
# All text after a single hash (#) is considered a comment and will be ignored.
# The format is:
# TAG = value [value, ...]
# For lists, items can also be appended using:
# TAG += value [value, ...]
#---------------------------------------------------------------------------
# Project related configuration options
#---------------------------------------------------------------------------
```

The header is the standard Doxygen-generated boilerplate (created
once by running `doxygen -g` and then heavily customised).

## Key tags M2 sets

The file is hundreds of lines of `TAG = value` settings.
Important customisations include (approximately):

| TAG | Set to | Why |
|---|---|---|
| `PROJECT_NAME` | "Macaulay2" | Title in generated HTML |
| `INPUT` | `@DOXYGEN_INPUT_DIR@` | Filled to `Macaulay2/e/` |
| `OUTPUT_DIRECTORY` | `@DOXYGEN_OUTPUT_DIR@` | Filled to a build-tree path |
| `EXTRACT_ALL` | `YES` | Document undocumented members too |
| `RECURSIVE` | `YES` | Walk subdirs of `e/` |
| `GENERATE_XML` | `YES` | For Breathe (Sphinx integration) |
| `XML_OUTPUT` | `xml` | Where XML lands |
| `LAYOUT_FILE` | `@CMAKE_SOURCE_DIR@/Macaulay2/e/doxygen-settings/DoxygenLayout.xml` | Custom layout |
| `HTML_EXTRA_STYLESHEET` | `.../doxygen-settings/*.css` | Custom styling |
| `CALL_GRAPH` | `@CALL_GRAPH_SETTING@` | Optional call graphs (requires `dot`) |

The `@VAR@` placeholders are CMake substitutions. The CMakeLists
([`file-CMakeLists.md`](file-CMakeLists.md)) runs
`configure_file(Doxyfile.in Doxyfile @ONLY)` to materialise the
real `Doxyfile`.

## Why a template?

The `INPUT` / `OUTPUT_DIRECTORY` paths depend on the user's
build tree (`M2/BUILD/build/...`). CMake knows them; Doxygen
doesn't. The template + `configure_file` is the standard way to
bridge.

## Doxygen → Breathe → Sphinx

```
Macaulay2/e/*.{h,hpp}
   ↓ doxygen (via Doxyfile)
xml/*.xml  +  html/*.html
   ↓ breathe (Sphinx extension)
RST-rendered Doxygen output
   ↓ sphinx-build
HTML in build dir
```

The XML output is what Breathe consumes; the HTML output is
linked directly from the Sphinx site (see
[`file-index-rst.md`](file-index-rst.md)).

## Used by

- Doxygen, when run by the `docs` CMake target.
- Indirectly, by Sphinx via the XML output.

## Related

- [`README.md`](README.md) — docs/ overview.
- [`file-conf-py.md`](file-conf-py.md) — Sphinx config that
  consumes Doxygen XML.
- [`file-index-rst.md`](file-index-rst.md) — entry point.
- [`../e/doxygen-settings/README.md`](../e/doxygen-settings/README.md)
  — layout + stylesheet overrides.
- [`../e/file-defgroups.md`](../e/file-defgroups.md) — `defgroups.h`
  defines the `@defgroup` categories.
