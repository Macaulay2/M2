# Documentation contribution guide

How to **add to or maintain** the M2 documentation tree. This
guide records the conventions the per-file deep dives, per-area
READMEs, and architecture references follow — so the tree stays
consistent as the codebase evolves.

[← repository TOC](README.md) · [Glossary](GLOSSARY.md) · [Tour](TOUR.md)

## Four levels of documentation

The tree has four layers (see
[`README.md`](README.md#documentation-map)):

1. **Top-level navigation** — [`README.md`](README.md).
2. **Per-directory READMEs** — every subdirectory has one.
3. **Per-layer / per-subdir architecture refs** —
   `architecture.md` files where complexity warrants.
4. **Per-file deep dives** — `file-<basename>.md` alongside each
   source file.

Plus three cross-cutting docs at the repo root:
[`GLOSSARY.md`](GLOSSARY.md), [`TOUR.md`](TOUR.md), this file.

## Naming conventions

### `file-<basename>.md`

A deep dive for `foo.cpp` (or `foo.hpp`, `foo.d`, `foo.m2`,
etc.) lives at `file-foo.md` in the same directory.

```
src/foo.cpp                    ←  the source file
src/file-foo.md                ←  its deep dive
src/README.md                  ←  the directory index
```

**For a `.cpp`+`.hpp` pair**: one deep dive named `file-foo.md`
covering both. Add `-cpp` / `-hpp` suffixes only when the two
files merit *separate* deep dives (rare).

**For shell scripts, Makefiles, configs**: use the canonical
basename. E.g. `Makefile.in` → `file-Makefile-in.md`;
`M2.in` → `file-M2-in.md`.

**For ambiguous bases**: when two source files would map to the
same deep-dive name (e.g. multiple `Makefile.in` files in nested
directories), the parent directory's name is implicit. The deep
dive's intro paragraph should disambiguate.

### Consolidated deep dives

When 3+ source files form a **cohesive family**, write one deep
dive with a descriptive name covering all of them:

| Consolidated deep dive | Covers |
|---|---|
| `bibasis/file-monom-orders.md` | `monomLex.{cpp,hpp}`, `monomDL.{cpp,hpp}`, `monomDRL.{cpp,hpp}` |
| `cmake/file-find-cmakes.md` | All 25 `Find*.cmake` modules |
| `cmake/file-misc-cmakes.md` | `prechecks`, `flavor`, `darwin`, `packaging`, `coverage`, `profiling`, `latex`, `stackcollapse-m2.sh` |
| `e/file-dmat-lu-variants.md` | `dmat-lu-inplace.hpp`, `dmat-lu-qq.hpp`, `dmat-lu-zzp-ffpack.hpp`, `dmat-lu-zzp-flint.hpp` |
| `e/file-res-old.md` | `res-a0.{cpp,hpp}`, `res-a1.{cpp,hpp}`, `res-a2.{cpp,hpp}` and their `-poly` / `-pair` / `-gb` companions |
| `e/file-gb-variants.md` | `gb-homog2.{cpp,hpp}`, `gb-sugarless.{cpp,hpp}`, `gb-toric.{cpp,hpp}`, `gb-walk.{cpp,hpp}` |
| `e/file-reducedgb.md` | `reducedgb-field.{cpp,hpp}`, `reducedgb-field-local.{cpp,hpp}`, `reducedgb-marked.{cpp,hpp}`, `reducedgb-ZZ.{cpp,hpp}` |
| `e/file-comp-gb-declared-proxy.md` | `comp-gb-declared.{cpp,hpp}`, `comp-gb-proxy.{cpp,hpp}` |
| `e/file-aring-zz-tests.md` | `ARingZZTest.cpp`, `ARingZZpTest.cpp`, `ARingQQGmpTest.cpp`, `ARingQQFlintTest.cpp` |
| ... | (see READMEs for full lists) |

Rule of thumb: a consolidated doc is right when the files share
the same algorithmic context and you'd be writing nearly the same
content for each one separately.

### `README.md`

Every directory has one. It serves as the **navigation index** for
the subdirectory. Structure:

    # `M2/path/to/dir/` — one-line description

    (optional) **See [`architecture·md`](architecture·md)** for the
    standalone architectural reference.

    Top-of-page paragraph(s) explaining the directory's role.

    [← back to ···](··/README·md)

    ## Files / Per-file deep dives / Subdirectories

    (tables here, with deep-dive links)

    **Coverage:** statement about per-file coverage.

    ## Related

    (cross-references)

(template uses `·` placeholders to keep literal dots; use real `.md` in actual files.)

### `architecture.md`

A higher-level architectural reference. Add one when a directory
contains 10+ source files and has structure worth explaining
above the per-file level. The existing 10 architecture refs are:

- **Per-layer** (4): `c/`, `d/`, `e/`, `m2/`.
- **Per-engine-subdir** (6): `interface/`, `f4/`, `gb-f4/`,
  `schreyer-resolution/`, `NCAlgebras/`, `bibasis/`.

`architecture.md` should:

- Open with a one-sentence description of what the directory
  contains.
- Have a "**position in the larger architecture**" section (a
  diagram showing how it fits with neighbours).
- Cover the key abstractions, data flow, memory model, threading
  concerns, etc.
- End with a **"how to extend"** section if applicable.
- Be **standalone-readable** — someone landing on it cold should
  understand the directory's purpose without reading other docs
  first.

## File structure of a per-file deep dive

    # `foo.cpp`, `foo.hpp` — one-line description

    (optional) opening paragraph framing this file's role in the
    larger area.

    Part of the [Area](area·md).

    [← per-area: area](area·md) · [← engine overview](README·md)

    ## What's declared / Header

        // excerpt of the actual header — usually the top 10-25 lines

    ## (section per major concern)

    (content)

    ## Used by

    - Specific upstream caller 1
    - Specific upstream caller 2

    ## Related

    - [`README·md`](README·md) — area overview.
    - [`file-relatedfile·md`](file-relatedfile·md) — sister file.
    - External library — link if applicable.

(template uses `·` placeholders for literal dots; replace with real `.md` in actual files.)

### What goes in "Used by"

Concrete, named callers. Not "the rest of the engine" but
specific file names / classes. The reader should be able to
follow the chain upstream.

### What goes in "Related"

Sister files (same area, similar role), supporting files (e.g.
the file's primary consumer or building block), external
libraries.

## Cross-reference rules

### Relative paths

All cross-references use **relative paths**, never absolute:

✅ `[file-foo·md](file-foo·md)` — same directory, relative.
✅ `[area·md](area·md)` — area overview, relative.
✅ `[../sibling-dir/README·md](../sibling-dir/README·md)` — relative across dirs.

❌ `[/M2/Macaulay2/e/foo·md](/M2/Macaulay2/e/foo·md)` — absolute path, fragile.
❌ `[`https://github.com/Macaulay2/M2/blob/main/...`](https://...)`

(External URLs are fine for external libraries / specs / issues.)

### Path-depth sanity

Common mistakes:

- From `M2/Macaulay2/e/<subdir>/`: submodules are at
  `../../../submodules/` (three levels up).
- From `M2/Macaulay2/e/`: docs/ is at `../docs/` (one level up,
  *not* `../../docs/`).
- From `M2/Macaulay2/e/<subdir>/`: docs/ is at `../../docs/`.

When in doubt: count the parent jumps.

### Cross-area references

References across language layers (`d/` ↔ `e/` ↔ `m2/`) are
common and good. Try to point at the most specific deep dive,
not just the area README.

## The README-sync rule

**Every time a new deep dive is added (or a per-directory README
is refined), update the top-level `README.md`'s table of contents
accordingly.**

This keeps the top-level `README.md` as a complete index of
everything reachable. The conventions:

- Each directory has a section in the top-level README's "deep
  dives" tables.
- New deep dives go into that section.
- New tables get added when a previously-undocumented directory
  starts getting deep dives.

The link-integrity audit (described below) catches when this
rule is violated.

## Link-integrity audit

Run this from the repo root to verify no broken markdown links:

```bash
broken=0
for readme in $(find M2 -name "*.md") README.md GLOSSARY.md TOUR.md CONTRIBUTING-DOCS.md; do
  dir=$(dirname "$readme")
  while IFS= read -r line; do
    link=$(echo "$line" | sed 's/](//;s/)$//')
    path="${link%#*}"
    if [ -n "$path" ] && [[ "$path" != http* ]] && [[ "$path" != mailto* ]]; then
      resolved="$dir/$path"
      if [ ! -f "$resolved" ] && [ ! -f "$path" ] && [ ! -d "$resolved" ]; then
        broken=$((broken+1))
        echo "BROKEN in $readme: $link"
      fi
    fi
  done < <(grep -oE '\]\([^)]+\.(md|cmake|hpp|cpp|h|c|d|dd|m2|y|l|sh|in|txt|css|js|py|el|svg|spec|ac|cff)\)' "$readme" 2>/dev/null)
done
echo "TOTAL BROKEN: $broken"
```

Should print `TOTAL BROKEN: 0`. If not, fix before merging.

## Style guidelines

### Use code formatting for source-file names

`foo.cpp`, not "foo.cpp". The backticks make filenames visually
distinct from prose.

### Open headers with an excerpt

Most deep dives open with the **actual source-file header** as a
code block (top 10-25 lines), so the reader sees what they'd see
in their editor. This grounds the abstract description in
concrete code.

### One-sentence file role at the top

The opening paragraph of a deep dive should answer "what is this
file" in one sentence. Save the detail for later sections.

### Prefer concrete to abstract

"This file handles X" is less useful than "This file's
`compute()` method processes one S-polynomial-batch by..." Be
specific.

### Cross-reference forwards and backwards

Each file's deep dive should link to:

- The directory README (its index).
- The area / architecture doc (its conceptual context).
- The specific files it interacts with (Used by, Related).
- External libraries it depends on.

## When to write a new deep dive

For an entirely new source file:

- **Always** write a `file-<basename>.md` deep dive.
- Add an entry to the directory's `README.md`.
- Add an entry to the top-level `README.md`'s relevant table.

For a moved / renamed file:

- Rename the deep dive accordingly.
- Update cross-references (grep for the old name).
- Update README tables.

For a deleted file:

- Delete the deep dive.
- Update cross-references (now-broken; the audit will catch
  them).

## When to write a new architecture doc

Add an `architecture.md` when a directory:

- Has 10+ source files, AND
- Has structural complexity above the per-file level (multiple
  algorithms, data structures, design patterns), AND
- A reader landing in the directory would benefit from a
  bird's-eye view before reading individual files.

The existing 10 architecture docs are good models. The first
few sections should always be:

- Opening: 1-paragraph description of what the dir contains.
- Position in the larger architecture (diagram).
- Layers or core abstractions (the meaty content).

## When to update GLOSSARY.md

Whenever you introduce or document a piece of M2-specific
terminology that crosses multiple files. Single-file jargon
belongs in that file's deep dive; cross-cutting terms belong in
the glossary.

## When to update TOUR.md

Add a new path when there's a clear audience persona not yet
covered (e.g. "writing a Doxygen extension," "implementing a new
build-system feature"). Existing paths should be updated when
their canonical reading order changes.

## Coverage banners

When a directory reaches 100% per-file deep-dive coverage, add a
banner to its README:

```markdown
**Coverage:** every source file in this directory has a dedicated deep-dive doc.
```

Consolidated docs count toward coverage: if `file-monom-orders.md`
covers three source files, those three are "covered."

## Avoiding documentation rot

Three classes of rot:

1. **Broken links** — caught by the audit above.
2. **Stale content** — a deep dive describes code that has since
   moved or been refactored.
3. **Missing entries** — new source files without matching deep
   dives.

Mitigations:

- **Run the link audit** on every PR that touches `.md` files.
- **Update the deep dive** when refactoring the corresponding
  source.
- **CI ideas** (not currently enforced): a check that every
  `.{cpp,hpp,d,dd,m2,c,h,y,l}` source file has a matching
  `file-*.md` (or is covered by a consolidated doc).

## Examples to model on

When writing a new deep dive, model it on an existing similar
one:

| New doc type | Model |
|---|---|
| Single-purpose source file | [`Macaulay2/d/file-evaluate.md`](M2/Macaulay2/d/file-evaluate.md) |
| Architectural class | [`Macaulay2/e/file-ring.md`](M2/Macaulay2/e/file-ring.md) |
| Consolidated family | [`Macaulay2/e/bibasis/file-monom-orders.md`](M2/Macaulay2/e/bibasis/file-monom-orders.md) |
| Template / generic code | [`Macaulay2/e/file-MemoryBlock.md`](M2/Macaulay2/e/file-MemoryBlock.md) |
| Build-system file | [`M2/cmake/file-configure-cmake.md`](M2/cmake/file-configure-cmake.md) |
| Architectural reference | [`M2/Macaulay2/e/architecture.md`](M2/Macaulay2/e/architecture.md) |

## Related

- [`README.md`](README.md) — repository TOC; the index docs sync
  with.
- [`GLOSSARY.md`](GLOSSARY.md) — terminology reference.
- [`TOUR.md`](TOUR.md) — audience-specific reading orders.
- Project [Wiki](https://github.com/Macaulay2/M2/wiki) and
  `.github/workflows/test_build.yml` — project-level build / test
  instructions (separate from documentation conventions).
