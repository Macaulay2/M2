# `M2/Macaulay2/docs/` — engine developer docs (Sphinx + Doxygen)

This directory holds the **developer-facing** documentation build for the C++
engine. Output is a Sphinx site (with Doxygen-extracted API references) aimed
at people hacking on `Macaulay2/e/`, *not* end users of the M2 language.

End-user M2 documentation is generated through an entirely different system —
the `document.m2` machinery in [`Macaulay2/m2/`](../m2/README.md), driven by
`installPackage` per package.

## Files

| File | Role | Deep dive |
|---|---|---|
| `conf.py` | Sphinx configuration | [`file-conf-py.md`](file-conf-py.md) |
| `index.rst`, `modules.rst` | RST entry points | [`file-index-rst.md`](file-index-rst.md) |
| `Doxyfile.in` | Templated Doxygen configuration | [`file-Doxyfile-in.md`](file-Doxyfile-in.md) |
| `CMakeLists.txt` | Sphinx + Doxygen build driver | [`file-CMakeLists.md`](file-CMakeLists.md) |

**Coverage:** every source file in this directory has a dedicated deep-dive doc.

## Building

The docs build is opt-in:

```sh
cmake -GNinja -S M2 -B M2/BUILD/build -DBUILD_DOCS=ON
cmake --build M2/BUILD/build --target docs
```

(Requires `sphinx-build` and `doxygen` on PATH. See
[`M2/cmake/FindSphinx.cmake`](../../cmake/FindSphinx.cmake) for the detection
logic.)

## Related

- [`Macaulay2/e/`](../e/README.md) — what the docs document.
- [`Macaulay2/man/`](../man/README.md) — installed man page (entirely separate).

[← back to repository TOC](../../../README.md#under-m2macaulay2)
