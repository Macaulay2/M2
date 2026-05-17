# `help.m2` — interactive help, `viewHelp`, `apropos`, `about`

`help.m2` is the **interactive help system** — the M2-side
implementation of `help`, `viewHelp`, `infoHelp`, `apropos`, and
`about`. It reads the documentation databases populated by
[`file-installPackage.md`](file-installPackage.md) and presents
nodes to the user at the M2 prompt.

Part of the [`m2/` Core layer](README.md).

[← back to m2 overview](README.md) · [← top-level engine TOC](../../../README.md)

## Exported entry points

```m2
-----------------------------------------------------------------------------
-- Methods for getting help and accessing the documentation
-----------------------------------------------------------------------------
-* Exported:
 * help
 * (symbol?, Thing)
 * viewHelp
 * infoHelp
 * apropos
 * about
 * pager
 *-

needs "system.m2"           -- for chkrun
needs "document.m2"
needs "installPackage.m2"   -- for topFileName
```

Seven user-facing functions:

- **`help x`** — display the doc node for `x` inline at the prompt.
- **`(symbol?, Thing)`** — operator form (`? x`) for quick lookup.
- **`viewHelp x`** — open the HTML doc for `x` in the user's
  browser.
- **`infoHelp x`** — open the info doc in `info`.
- **`apropos pat`** — list M2 symbols matching `pat`.
- **`about pat`** — search doc-node bodies for `pat`.
- **`pager s`** — display a long string `s` paginated.

## Where lookup goes

When a user asks `help` for something:

1. **In-memory docs** — if the package containing the node is
   currently loaded, use its in-memory doc database (populated by
   `document.m2`).
2. **On-disk doc databases** — fall back to the `gdbm` databases
   `installPackage` wrote.
3. **Best-effort autocomplete** — if no exact match, suggest nearby
   doc nodes via fuzzy matching.

## `apropos` vs `about`

| Function | Searches | Speed |
|---|---|---|
| `apropos` | Symbol names | Fast (in-memory lookup) |
| `about` | Doc-node bodies (full text) | Slower (walks all docs) |

Users typically reach for `apropos` first; `about` is for harder
"I forget the name but I remember it had to do with X" searches.

## Cross-package help

Macaulay2 packages can cross-reference each other's docs via `TO`
links. When the user follows such a link in `viewHelp`, the
HTML-side cross-reference takes them to the target package's
generated HTML.

## Related

- [`README.md`](README.md) — m2/ overview.
- [`file-document.md`](file-document.md) — input DSL.
- [`file-installPackage.md`](file-installPackage.md) — produces the
  on-disk databases this file reads.
- `system.m2` — `chkrun` for launching external viewers
  (`viewHelp`, `infoHelp`).
- `Browse` package — auto-installed; gives `viewHelp` a richer GUI.
