# `M2/Macaulay2/e/doxygen-settings/` — Doxygen presentation assets

Doxygen configuration overrides and stylesheet customisations for the
engine's C++ API documentation. Consumed by the Doxygen run wired into the
Sphinx build under [`Macaulay2/docs/`](../../docs/README.md).

| File | Role |
|---|---|
| `Doxyconf` | Doxygen configuration fragments overlaid on the templated `Doxyfile.in` |
| `DoxygenLayout.xml` | Top-level layout of the generated HTML |
| `navtree.css`, `styleSheetFile.css`, `tabs.css` | Visual styling for the generated docs |

If you want the API docs to look different (sidebar layout, colors, fonts),
this is where to edit.

## Related

- [`Macaulay2/docs/`](../../docs/README.md) — the Sphinx + Doxygen build that
  uses these files.

[← back to engine overview](../README.md)
