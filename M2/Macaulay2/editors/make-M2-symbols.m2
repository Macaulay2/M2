-------------------------------------------------------------------------------
-- This script is responsible for creating a list of all builtin symbols, such
-- as keywords, types, etc. and substituting them in grammar files for various
-- editors and syntax highlighting engines. Grammar file templates are assumed
-- to be located in the same directory as this script.
-------------------------------------------------------------------------------

needsPackage "Style"

-- Emacs: Write M2-symbols.el
generateGrammar("emacs/M2-symbols.el", x -> demark(" ", format \ x))

-- Atom & Linguist: Write macaulay2.cson
generateGrammar("atom/macaulay2.cson", x -> demark("|", x))

-- Prism: Write macaulay2.js
generateGrammar("prism/macaulay2.js", x -> demark("|", x))

-- Vim: Write m2.vim.syntax and m2.vim.dict
generateGrammar("vim/m2.vim.syntax", x -> demark(" ", x))
generateGrammar("vim/m2.vim.dict", x -> demark(" ", x)) -- TODO: is this necessary?

-- Pygments: Write macaulay2.py
generateGrammar("pygments/macaulay2.py",
    x -> demark("," | newline | "    ", format \ x))

-- highlight.js: Write macaulay2.js
generateGrammar("highlightjs/macaulay2.js",
    x -> demark("," | newline | "	", format \ x))

generateGrammar("textmate/macaulay2.tmLanguage.json", x -> demark("|", x))

-- Macaulay2Web: Write M2-symbols.ts
generateGrammar("Macaulay2Web/M2-symbols.ts", x -> demark(",", format \ x))

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/emacs M2-symbols "
-- End:
