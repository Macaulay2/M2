-------------------------------------------------------------------------------
-- This script is responsible for creating a list of all builtin symbols, such
-- as keywords, types, etc. and substituting them in grammar files for various
-- editors and syntax highlighting engines. Grammar file templates are assumed
-- to be located in the same directory as this script.
-------------------------------------------------------------------------------

needsPackage "Style"

-- Emacs: Write M2-symbols.el
generateGrammar("emacs/M2-symbols.el", x -> demark(" ", format \ x))

-- Prism: Write macaulay2.js
generateGrammar("prism/macaulay2.js", x -> demark("|", x))

-- Vim: Write syntax/m2.vim and dict/m2.vim.dict
generateGrammar("vim/syntax/m2.vim", x -> demark(" ", x))
generateGrammar("vim/dict/m2.vim.dict", x -> demark(" ", x))

-- Pygments: Write macaulay2.py
generateGrammar("pygments/macaulay2.py",
    x -> demark("," | newline | "    ", format \ x))

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/emacs M2-symbols "
-- End:
