-------------------------------------------------------------------------------
-- This script is responsible for creating a list of all builtin symbols, such
-- as keywords, types, etc. and substituting them in grammar files for various
-- editors and syntax highlighting engines. Grammar file templates are assumed
-- to be located in the same directory as this script.
-- Currently:
--  - Emacs
--  - Atom & Linguist: https://github.com/Macaulay2/language-macaulay2
--  - Rouge

-------------------------------------------------------------------------------
-- TODO: Move these two elsewhere:
Function and Function := (f, g) -> s -> f s and g s
Function or  Function := (f, g) -> s -> f s or  g s

is := X -> (name, symb) -> instance(value symb, X)

isAlpha        := s -> match("^[[:alpha:]]+$", s)
isAlphaNumeric := s -> match("^[[:alnum:]]+$", s)

-- Things we want to highlight:
isType     := is Type
isKeyword  := is Keyword
isFunction := is Function
isConst    := (name, symb) -> (isAlpha name
    and not (isFunction or isType or isKeyword) (name, symb)
    and (symb === symbol null or value symb =!= null))

okay := method()
okay(String, Keyword) :=
okay(String, Symbol)  := (name, pkg) -> length name > 1 and isAlphaNumeric name

-------------------------------------------------------------------------------
-- Get a list of all symbols visible just after loading preloaded packages
allPkgNames := separate(" ", version#"packages") | {"Core"}
loadedPkgNames := Core#"pre-installed packages" | {"Core", "Text", "Parsing", "SimpleDoc"}
symbols := unique sort join(
    apply(allPkgNames, pkgname -> (pkgname, symbol Core)),
    flatten apply(loadedPkgNames, pkgname -> (
	    pkg := needsPackage pkgname;
	    select(pairs pkg.Dictionary, okay))))

if length symbols < 1500 then error "expected more entries for M2-symbols"

-- Check for invalid symbols
bad := select(symbols, (name, symb) -> not okay(name, symb))
if #bad > 0 then error(
    "encountered symbol(s) that are not alphanumeric or have less than 2 characters:",
    concatenate apply(bad, (name, symb) ->
	{"\n\t", -* TODO: symbolLocation name, ": here is the first use of ", *- toString name}))

-------------------------------------------------------------------------------
-- Put the symbols into bins
SYMBOLS   = first \ select(symbols, (name, symb) -> isAlphaNumeric name)
KEYWORDS  = first \ select(symbols, isKeyword)
DATATYPES = first \ select(symbols, isType)
FUNCTIONS = first \ select(symbols, isFunction)
CONSTANTS = first \ select(symbols, isConst)
CONSTANTS = CONSTANTS | {"Node", "Item", "Example", "CannedExample", "Pre", "Code", "Tree"} -- SimpleDoc words
CONSTANTS = sort CONSTANTS
STRINGS   = format "///\\\\(/?/?[^/]\\\\|\\\\(//\\\\)*////[^/]\\\\)*\\\\(//\\\\)*///"

-------------------------------------------------------------------------------
-- Substitute symbols, keywords, types, functions, and constants

-- This banner is added to the top of generated grammars
banner := "Auto-generated for Macaulay2-@M2VERSION@. Do not modify this file manually."

symbolsForVim = template -> (
    output := concatenate("\"\" ", banner, newline, newline, template);
    output = replace("@M2VERSION@",   version#"VERSION",      output);
    output = replace("@M2SYMBOLS@",   demark(" ", SYMBOLS),   output);
    output = replace("@M2KEYWORDS@",  demark(" ", KEYWORDS),  output);
    output = replace("@M2DATATYPES@", demark(" ", DATATYPES), output);
    output = replace("@M2FUNCTIONS@", demark(" ", FUNCTIONS), output);
    output = replace("@M2CONSTANTS@", demark(" ", CONSTANTS), output);
    output = replace("@M2STRINGS@",               STRINGS,    output);
    output)

symbolsForEmacs = template -> (
    output := concatenate(";; ", banner, newline, newline, template);
    output = replace("@M2VERSION@",   version#"VERSION",               output);
    output = replace("@M2SYMBOLS@",   demark(" ", format \ SYMBOLS),   output);
    output = replace("@M2KEYWORDS@",  demark(" ", format \ KEYWORDS),  output);
    output = replace("@M2DATATYPES@", demark(" ", format \ DATATYPES), output);
    output = replace("@M2FUNCTIONS@", demark(" ", format \ FUNCTIONS), output);
    output = replace("@M2CONSTANTS@", demark(" ", format \ CONSTANTS), output);
    output = replace("@M2STRINGS@",                        STRINGS,    output);
    output)

symbolsForAtom = template -> (
    output := concatenate("## ", banner, newline, newline, template);
    output = replace("@M2VERSION@",   version#"VERSION",      output);
    --output = replace("@M2SYMBOLS@",   demark("|", SYMBOLS),   output);
    output = replace("@M2KEYWORDS@",  demark("|", KEYWORDS),  output);
    output = replace("@M2DATATYPES@", demark("|", DATATYPES), output);
    output = replace("@M2FUNCTIONS@", demark("|", FUNCTIONS), output);
    output = replace("@M2CONSTANTS@", demark("|", CONSTANTS), output);
    output = replace("@M2STRINGS@",               STRINGS,    output);
    output)

symbolsForPrism = template -> (
    output := concatenate("// ", banner, newline, newline, template);
    output = replace("@M2VERSION@",   version#"VERSION",      output);
    output = replace("@M2SYMBOLS@",   demark(",", format \ SYMBOLS), output);
    output = replace("@M2KEYWORDS@",  demark("|", KEYWORDS),  output);
    output = replace("@M2DATATYPES@", demark("|", DATATYPES), output);
    output = replace("@M2FUNCTIONS@", demark("|", FUNCTIONS), output);
    output = replace("@M2CONSTANTS@", demark("|", CONSTANTS), output);
    output = replace("@M2STRINGS@",               STRINGS,    output);
    output)

symbolsForRouge = template -> (
    output := concatenate("## ", banner, newline, newline, template);
    output = replace("@M2VERSION@",   version#"VERSION",      output);
    --output = replace("@M2SYMBOLS@",   demark("|", SYMBOLS),   output);
    output = replace("@M2KEYWORDS@",  demark(" ", KEYWORDS),  output);
    output = replace("@M2DATATYPES@", demark("|", DATATYPES), output);
    output = replace("@M2FUNCTIONS@", demark("|", FUNCTIONS), output);
    output = replace("@M2CONSTANTS@", demark("|", CONSTANTS), output);
    output = replace("@M2STRINGS@",               STRINGS,    output);
    output)

-------------------------------------------------------------------------------
-- Generate syntax files from templates in the same directory

generateGrammar := (grammarFile, grammarFunction) -> (
    template := currentFileDirectory | grammarFile | ".in";
    if fileExists template then (
        stdio << "-- Generating " << grammarFile << endl;
        directory := replace("/[^/].*$", "", grammarFile);
        if not isDirectory directory then makeDirectory directory;
        grammarFile << grammarFunction get(template) << close)
    else stderr << "Skipping generation of " << grammarFile << " as it does not exist." << endl;)

-- Emacs: Write M2-symbols.el
generateGrammar("emacs/M2-symbols.el", symbolsForEmacs)

-- Atom & Linguist: Write macaulay2.cson
generateGrammar("atom/macaulay2.cson", symbolsForAtom);

-- Prism: Write macaulay2.js
generateGrammar("prism/macaulay2.js", symbolsForPrism);

-- Vim: Write m2.vim.syntax and m2.vim.dict
generateGrammar("vim/m2.vim.syntax", symbolsForVim);
generateGrammar("vim/m2.vim.dict", symbolsForVim); -- TODO: is this necessary?

-- Rouge: Write macaulay2.rb
--generateGrammar("rouge/macaulay2.rb", symbolsForRouge);

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/emacs M2-symbols "
-- End:
