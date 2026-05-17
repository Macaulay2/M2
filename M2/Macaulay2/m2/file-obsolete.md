# `obsolete.m2` — placeholder for deprecated functions

`obsolete.m2` is the **placeholder file** Macaulay2 maintains for
marking functions as obsolete / deprecated without immediately
removing them. As of the current source, the file is effectively
empty — there's no currently-deprecated symbol to mark.

Part of the [`m2/` Core layer](README.md).

[← back to m2 overview](README.md) · [← top-level engine TOC](../../../README.md)

## Full content

```m2
--		Copyright 1997-2002 by Daniel R. Grayson

-- preserve this file, in case we want to remove a function and mark it obsolete:

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
```

That's the whole file. The "preserve this file, in case…" comment
explains its purpose: keep it around so future deprecations have a
clear home.

## When the file gets content

A typical deprecation lifecycle:

1. A function `foo` is identified as deprecated.
2. Its body is moved into `obsolete.m2` and wrapped with a
   "deprecated, use `bar` instead" warning.
3. After a release cycle or two, the function is fully removed.

Step 2's wrapper looks like:

```m2
foo = (args) -> (
    stderr << "warning: foo is deprecated; use bar instead" << endl;
    bar args
)
```

## Why a separate file

Without `obsolete.m2`, deprecations would scatter — some symbols
deprecated in one file, others in another. Centralising here:

- Makes it easy to grep "what's currently deprecated?"
- Lets the maintainers see at a glance how much deprecation debt is
  outstanding.
- Provides a single deletion point when retiring symbols.

## Related

- [`README.md`](README.md) — m2/ overview.
- [`file-loadsequence.md`](file-loadsequence.md) — `obsolete.m2`
  loads near the end (after most things it might deprecate).
- The `*-obsolete.m2` files in
  [`../packages/`](../packages/README.md) — per-package deprecation
  files following the same pattern.
