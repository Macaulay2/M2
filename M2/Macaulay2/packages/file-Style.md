# `Style.m2` — documentation stylesheets, images, grammar generation

The `Style` package supplies **the visual styling and assets**
for M2's generated HTML documentation — CSS, JS, images — plus
the **`generateGrammar` function** the [`editors/`](../editors/README.md)
build relies on to emit per-editor syntax files.

Part of [`packages/`](README.md).

[← packages/ overview](README.md) · [← top-level engine TOC](../../../README.md)

## Header

```m2
-- -*- coding: utf-8 -*-
newPackage( "Style",
     AuxiliaryFiles => true,
     Headline => "style sheets and images for the documentation",
     Keywords => {"Documentation"},
     Version => "1.1"
     )

---------------------------------------------
-- grammar generation code adapted from    --
-- M2/Macaulay2/editors/make-M2-symbols.m2 --
---------------------------------------------

export {"generateGrammar"}

importFrom(Core, "sortBy")
```

The first surprise: this package's name is **`Style`** (visual
styling) but it also **exports `generateGrammar`** — a grammar
generator for syntax highlighters. The historical logic:

1. Both styling and grammar generation are concerns of "how M2
   looks in external tools."
2. Both need the same Core-symbol introspection.
3. Splitting into two packages would add complexity for no win.

So they live together.

## `generateGrammar`

The exported function the `editors/` build calls:

```m2
generateGrammar("emacs/M2-symbols.el", x -> demark(" ", format \ x))
generateGrammar("prism/macaulay2.js", x -> demark("|", x))
generateGrammar("vim/m2.vim.syntax", x -> demark(" ", x))
generateGrammar("pygments/macaulay2.py",
    x -> demark("," | newline | "    ", format \ x))
```

For each call:

1. Walk every symbol in `Core#"private dictionary"` and the
   public Core dictionary.
2. Bucket by symbol kind (Type, Keyword, Function, constant).
3. Format each bucket with the caller's formatter callback.
4. Substitute the formatted symbol lists into the `.in`
   template at the named path.
5. Write the output file.

## Symbol classification

```m2
is := X -> (name, symb) -> instance(value symb, X)

isType     := is Type
isKeyword  := is Keyword
isFunction := is Function
isConst    := (name, symb) -> (isAlphaNumeric name
    and not (isFunction or isType or isKeyword) (name, symb)
    and (symb === symbol null or value symb =!= null))
```

The `is` factory builds a per-class predicate. After running, the
package knows exactly which symbols are types, keywords,
functions, and constants — that drives grammar bucketing.

## Auxiliary files

The `Style/` subdir holds the **non-M2 assets**:

```
Style/
├── doc.css         (HTML doc styling)
├── prism.css       (syntax highlighting)
├── M2-logo.svg     (the logo)
├── icons/          (interface icons)
└── ...
```

These get copied into the generated HTML tree during
`installPackage`. Users see them in the doc viewer.

## Used by

- [`Macaulay2Doc`](file-Macaulay2Doc.md) — for its generated HTML.
- Every other package's HTML — they all share the Style assets.
- [`../editors/file-make-M2-symbols.md`](../editors/file-make-M2-symbols.md)
  — calls `generateGrammar` during build.

## Related

- [`README.md`](README.md) — packages/ overview.
- [`file-Macaulay2Doc.md`](file-Macaulay2Doc.md) — primary HTML
  consumer.
- [`../editors/file-make-M2-symbols.md`](../editors/file-make-M2-symbols.md)
  — `generateGrammar` consumer.
- [`../m2/file-installPackage.md`](../m2/file-installPackage.md)
  — invokes the HTML generation Style supplies for.
