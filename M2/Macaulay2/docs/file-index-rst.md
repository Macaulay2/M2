# `index.rst`, `modules.rst` — Sphinx entry points

`index.rst` is the **top-level Sphinx page** for the engine's
internal developer docs. `modules.rst` is the second-level
"Modules" index it pulls in.

Part of [`docs/`](README.md).

[← docs/ overview](README.md) · [← top-level engine TOC](../../../README.md)

## `index.rst` — the landing page

```rst
Macaulay2: Internal Documentation
=================================

* Repository: https://github.com/Macaulay2/M2
* Issue tracker: https://github.com/Macaulay2/M2/issues
* Mailing list: https://groups.google.com/group/macaulay2
* `Doxygen index <doxygen/html/index.html>`_

.. toctree::
   :maxdepth: 2
   :caption: Contents:

   modules.rst
..   namespaces.rst
     classes.rst
     files.rst

.. doxygenfile:: engine.h

Indices and tables
==================

* :ref:`genindex`
* :ref:`search`
```

The landing page links out to:

- **Repository** — GitHub.
- **Issue tracker**.
- **Mailing list** — Google Groups.
- **Doxygen index** — the raw Doxygen-generated HTML (linked
  directly, not Sphinx-rendered).

The `.. toctree::` directive shows what subsections exist;
`modules.rst` is the only one included by default. The
commented-out `namespaces.rst`/`classes.rst`/`files.rst` lines
show that those were planned but not (yet) generated.

The `.. doxygenfile:: engine.h` directive **embeds the entire
`engine.h` C API** rendered through Doxygen → Breathe → Sphinx.
Picking `engine.h` is deliberate: it's the most important file
for an engine consumer to know about.

## `modules.rst` — module index

```rst
Macaulay2: Modules
=================================

.. toctree::
   :maxdepth: 1

..   modules.rst
     namespaces.rst
     classes.rst
     files.rst
```

Currently a **stub** — the toctree references files that aren't
generated yet. Future work to expand:

- Auto-generate `namespaces.rst`, `classes.rst`, `files.rst` from
  Doxygen XML via Breathe.
- Add narrative chapters per engine area (rings, matrices, GB,
  resolutions, ...).

The fact that this is a stub matches the doc's stated audience
(engine developers): the narrative coverage in the markdown
deep-dives across the repo (this very file system) is *more*
detailed than the Sphinx build would render.

## Why Sphinx if it's incomplete?

Three reasons it's kept around:

1. **The Doxygen output** is genuinely useful — class diagrams,
   inheritance trees.
2. **The `index.html` linkable URL** is a stable entry point.
3. **Easy to grow** — if someone wants to add a chapter, the
   skeleton is here.

## Used by

- Sphinx, building the HTML site.
- The `docs` CMake target.

## Related

- [`README.md`](README.md) — docs/ overview.
- [`file-conf-py.md`](file-conf-py.md) — Sphinx config.
- [`file-Doxyfile-in.md`](file-Doxyfile-in.md) — Doxygen config.
- [`../e/file-engine-h.md`](../e/file-engine-h.md) — the file
  embedded in `index.rst`.
