# `CMakeLists.txt` — Sphinx + Doxygen build driver

`CMakeLists.txt` is the **CMake build driver** that wires Doxygen
and Sphinx together to produce the engine's internal-documentation
HTML site.

Part of [`docs/`](README.md).

[← docs/ overview](README.md) · [← top-level engine TOC](../../../README.md)

## Header

```cmake
# Guide: https://devblogs.microsoft.com/cppblog/clear-functional-c-documentation-with-sphinx-breathe-doxygen-cmake/
# Requirement:
#   pip install breathe

# Sphinx and Doxygen options
set(SPHINX_OPTIONS	"" CACHE STRING "...")
set(DOXYGEN_OPTIONS	"" CACHE STRING "...")
set(CALL_GRAPH_SETTING	NO CACHE BOOL "...")
set(HAVE_DOT_SETTING	NO CACHE BOOL "...")

set(DOXYFILE_IN ${CMAKE_CURRENT_SOURCE_DIR}/Doxyfile.in)
set(DOXYFILE_OUT ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile)
set(DOXYGEN_INPUT_DIR ${CMAKE_SOURCE_DIR}/Macaulay2/e)
set(DOXYGEN_OUTPUT_DIR ${CMAKE_CURRENT_BINARY_DIR}/doxygen)
set(DOXYGEN_INDEX_FILE ${DOXYGEN_OUTPUT_DIR}/xml/index.xml)

set(SPHINX_INPUT_DIR ${CMAKE_CURRENT_SOURCE_DIR})
set(SPHINX_OUTPUT_DIR ${CMAKE_CURRENT_BINARY_DIR})
set(SPHINX_INPUT_DIR ${CMAKE_CURRENT_SOURCE_DIR})
set(SPHINX_OUTPUT_DIR ${CMAKE_CURRENT_BINARY_DIR})
set(SPHINX_INDEX_FILE ${SPHINX_OUTPUT_DIR}/index.html)

file(GLOB_RECURSE DOXYGEN_SOURCES ${DOXYGEN_INPUT_DIR}/*.h ${DOXYGEN_INPUT_DIR}/*.hpp)
file(GLOB_RECURSE SPHINX_SOURCES  ${SPHINX_INPUT_DIR}/*.rst)

file(MAKE_DIRECTORY ${DOXYGEN_OUTPUT_DIR})
configure_file(Doxyfile.in Doxyfile @ONLY)
```

The header link (Microsoft's devblog) is the **canonical recipe**
for Sphinx + Breathe + Doxygen — M2's setup follows it closely.

## Variables set up

| Variable | Holds |
|---|---|
| `DOXYFILE_IN` | path to `Doxyfile.in` template |
| `DOXYFILE_OUT` | path to materialised `Doxyfile` |
| `DOXYGEN_INPUT_DIR` | engine source: `Macaulay2/e/` |
| `DOXYGEN_OUTPUT_DIR` | `<build>/Macaulay2/docs/doxygen/` |
| `DOXYGEN_INDEX_FILE` | the marker file that says "Doxygen ran" |
| `SPHINX_INPUT_DIR` | this dir (`Macaulay2/docs/`) |
| `SPHINX_OUTPUT_DIR` | `<build>/Macaulay2/docs/` |
| `SPHINX_INDEX_FILE` | the marker file that says "Sphinx ran" |

The `file(GLOB_RECURSE ...)` calls list dependencies — every `.h`
/ `.hpp` in `e/` and every `.rst` in this dir. When any of these
change, the doc build re-runs.

## Build targets

The rest of the file (not shown) sets up:

- **`docs-doxygen`** — runs Doxygen on the engine sources.
- **`docs-sphinx`** — runs Sphinx, depends on docs-doxygen.
- **`docs`** — convenience target depending on docs-sphinx.

## Optional features

- **Call graphs** (`CALL_GRAPH_SETTING`) — only enabled if the
  user opts in *and* `dot` (from Graphviz) is available.
- **Cached options** — `SPHINX_OPTIONS` and `DOXYGEN_OPTIONS`
  let the user pass extra flags without editing this file.

## Opt-in build

The docs target is **not built by default** — most users don't
need them. To enable:

```sh
cmake -GNinja -S M2 -B M2/BUILD/build -DBUILD_DOCS=ON
cmake --build M2/BUILD/build --target docs
```

Requires `sphinx-build`, `doxygen`, and `breathe` (pip-installed).

## Used by

- The CMake build, when `-DBUILD_DOCS=ON`.

## Related

- [`README.md`](README.md) — docs/ overview.
- [`file-Doxyfile-in.md`](file-Doxyfile-in.md) — template
  consumed here.
- [`file-conf-py.md`](file-conf-py.md) — Sphinx config consumed
  here.
- `M2/cmake/FindSphinx.cmake` — Sphinx detection.
