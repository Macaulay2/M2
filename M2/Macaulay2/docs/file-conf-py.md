# `conf.py` — Sphinx configuration

`conf.py` is the **Sphinx configuration** for the engine
developer-facing C++ documentation build. Standard Sphinx layout
with the additions M2 needs (Doxygen integration via Breathe).

Part of [`docs/`](README.md).

[← docs/ overview](README.md) · [← top-level engine TOC](../../../README.md)

## Header

```python
# Configuration file for the Sphinx documentation builder.
#
# This file only contains a selection of the most common options. For a full
# list see the documentation:
# http://www.sphinx-doc.org/en/master/config

# -- Path setup --------------------------------------------------------------

# If extensions (or modules to document with autodoc) are in another directory,
# add these directories to sys.path here. If the directory is relative to the
# documentation root, use os.path.abspath to make it absolute, like shown here.
#
# import os
# import sys
# sys.path.insert(0, os.path.abspath('.'))


# -- Project information -----------------------------------------------------

project = 'Macaulay2 Internal Documentation'
copyright = '2020, The Macaulay2 Authors'
author = 'The Macaulay2 Authors'
```

Standard Sphinx boilerplate. The "**Internal Documentation**"
title makes the audience clear — this is for engine hackers, not
M2 end users.

## What's configured

Beyond what's shown, `conf.py` typically configures:

- **Extensions** — `breathe` for Doxygen XML ingestion, possibly
  `sphinx.ext.autodoc`, `sphinx.ext.viewcode`.
- **Theme** — typically `sphinx_rtd_theme` or default.
- **Breathe project** — points at the Doxygen XML output:

  ```python
  breathe_projects = { "Macaulay2": "doxygen/xml/" }
  breathe_default_project = "Macaulay2"
  ```

- **Source suffix** — `.rst`.
- **Master doc** — `'index'`.

## How the pipeline works

```
Doxygen reads Macaulay2/e/*.{h,hpp} 
    ↓ produces doxygen/xml/*.xml
Breathe (Sphinx extension) reads the XML
    ↓ exposes \\doxygenclass{}, \\doxygenfile{} directives
index.rst uses .. doxygenfile:: engine.h
    ↓
Sphinx renders to HTML
```

So `conf.py` is the **bridge between Doxygen output and the
Sphinx HTML build**.

## Why Sphinx + Doxygen

Either alone could work:

- **Sphinx only** — but you'd lose Doxygen's automatic class /
  function extraction from C++ headers.
- **Doxygen only** — but its HTML output looks dated and isn't
  composable with hand-written RST text.

Together: Doxygen handles "automatically document every
class/function in `e/`," Sphinx handles "wrap that in a readable
narrative."

## Build

```sh
cmake --build M2/BUILD/build --target docs
```

The CMake target ([`file-CMakeLists.md`](file-CMakeLists.md))
runs Doxygen then Sphinx in sequence.

## Used by

- Sphinx, when running `sphinx-build`.
- The `docs` CMake target.

## Related

- [`README.md`](README.md) — docs/ overview.
- [`file-Doxyfile-in.md`](file-Doxyfile-in.md) — Doxygen config.
- [`file-index-rst.md`](file-index-rst.md) — entry point.
- Sphinx, Breathe, Doxygen — external tools.
