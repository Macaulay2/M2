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

is := X -> (name, symb) -> instance(value symb, X)

isAlpha        := s -> match("^[[:alpha:]]+$", s)
isAlphaNumeric := s -> match("^[[:alnum:]]+$", s)

-- Things we want to highlight:
isType     := is Type
isKeyword  := is Keyword
isFunction := is Function
isConst    := (name, symb) -> (isAlphaNumeric name
    and not (isFunction or isType or isKeyword) (name, symb)
    and (symb === symbol null or value symb =!= null))

okay := method()
okay(String, Keyword) :=
okay(String, Symbol)  := (name, pkg) -> length name > 1 and isAlphaNumeric name

cachedSymbols = new CacheTable
STRINGS = format "///\\\\(/?/?[^/]\\\\|\\\\(//\\\\)*////[^/]\\\\)*\\\\(//\\\\)*///"

-- This banner is added to the top of generated grammars
banner := "Auto-generated for Macaulay2-@M2VERSION@. Do not modify this file manually."

generateSymbols = () -> (
    ----------------------------------------------------------------------------
    -- Get a list of all symbols visible just after loading preloaded packages
    allPkgNames := separate(" ", version#"packages") | {"Core"};
    loadedPkgNames := join(Core#"preloaded packages",
	{"Core", "Text", "Parsing", "SimpleDoc"});
    symbols := unique (sortBy ascii @@ first) join(
	apply(allPkgNames, pkgname -> (pkgname, symbol Core)),
	flatten apply(loadedPkgNames, pkgname -> (
		pkg := needsPackage pkgname;
		select(pairs pkg.Dictionary, okay))));

    if length symbols < 1500 then error "expected more entries for M2-symbols";

    -------------------------------------------------------------------------------
    -- Put the symbols into bins
    cachedSymbols.Symbol   = first \ select(symbols,
	(name, symb) ->isAlphaNumeric name);
    cachedSymbols.Keyword  = first \ select(symbols, isKeyword);
    cachedSymbols.Type     = first \ select(symbols, isType);
    cachedSymbols.Function = first \ select(symbols, isFunction);
    cachedSymbols.Constant = sort join(
	first \ select(symbols, isConst), {
	    -- SimpleDoc docstring keywords -- keep updated
	    -- NodeFunctions
	    "Node",
	    "Synopsis",
	    -- DescriptionFunctions
	    "CannedExample",
	    "Code",
	    "Example",
	    "Pre",
	    "Tree",
	    -- ConsequenceFunctions
	    "Item"
	    });
    )

-------------------------------------------------------------------------------
-- Generate syntax files from templates in the same directory

generateGrammar = method()
generateGrammar(String, String, Function) := (template, outfile, demarkf) -> (
    if not fileExists template
    then (
	printerr("warning: ", template,
	    " does not exist; skipping generation of ", outfile);
	return);
    printerr("generating ", outfile);
    directory := replace("/[^/].*$", "", outfile);
    if not isDirectory directory then makeDirectory directory;
    output := get template;
    if #cachedSymbols == 0 then generateSymbols();
    output = replace("@M2BANNER@",    banner,                         output);
    output = replace("@M2VERSION@",   version#"VERSION",              output);
    output = replace("@M2SYMBOLS@",   demarkf cachedSymbols.Symbol,   output);
    output = replace("@M2KEYWORDS@",  demarkf cachedSymbols.Keyword,  output);
    output = replace("@M2DATATYPES@", demarkf cachedSymbols.Type,     output);
    output = replace("@M2FUNCTIONS@", demarkf cachedSymbols.Function, output);
    output = replace("@M2CONSTANTS@", demarkf cachedSymbols.Constant, output);
    output = replace("@M2STRINGS@",   STRINGS,                        output);
    outfile << output << close;)
generateGrammar(String, Function) := (outfile, demarkf) -> (
    generateGrammar(currentFileDirectory | outfile | ".in", outfile, demarkf))

beginDocumentation()

doc ///
  Key
    Style
  Headline
    style sheets and images for the documentation
  Description
    Text
      This package is primarily a repository for the style sheets and images
      used by the html pages in the documentation.

      It exports one method, @TO generateGrammar@, for generating grammar files
      used for syntax highlighting and/or auto-completion in Macaulay2 code
      editors and in the html documentation.
///

doc ///
  Key
    generateGrammar
    (generateGrammar, String, Function)
    (generateGrammar, String, String, Function)
  Headline
    generate a grammar file from a template
  Usage
    generateGrammar(template, outfile, demarkf)
    generateGrammar(outfile, demarkf)
  Inputs
    template:String -- the filename of the template file
    outfile:String -- the filename of the output file
    demarkf:Function -- how to demark a list of symbols
  Description
    Text
      This method generates a grammar file for use in syntax highlighting
      Macaulay2 code in a text editor or web browser.

      The file @SAMP "template"@ may contain any of the following special
      strings:

      @UL {
	  LI {SAMP "\100M2BANNER\100", ", for placing a banner mentioning that
	      the resulting file was automatically generated."},
	  LI {SAMP "\100M2VERSION\100", ", for the current version of ",
	      "Macaulay2."},
	  LI {SAMP "\100M2SYMBOLS\100", ", for a list of all Macaulay2 symbols",
	      ", e.g., for automatic completion."},
	  LI {SAMP "\100M2KEYWORDS\100", ", for a list of Macaulay2 keywords."},
	  LI {SAMP "\100M2DATATYPES\100", ", for a list of Macaulay2 types."},
	  LI {SAMP "\100M2FUNCTIONS\100", ", for a list of Macaulay2 ",
	      "functions."},
	  LI {SAMP "\100M2CONSTANTS\100", ", for a list of Macaulay2 symbols ",
	      "and packages."},
	  LI {SAMP "\100M2STRINGS\100", ", for a regular expression that ",
	      "matches Macaulay2 strings."}}@

      The function @SAMP "demarkf"@ indicates how the elements of each of the
      lists will be demarked in the resulting file.   The file @SAMP "outfile"@
      will then be generated, replacing each of these strings as indicated
      above.
    Example
      outfile = temporaryFileName()
      template = outfile | ".in"
      template << "@M2BANNER@" << endl << endl;
      template << "This is an example file for the generateGrammar method!";
      template << endl;
      template << "String regex: @M2STRINGS@" << endl;
      template << "List of keywords: {" << endl;
      template << "    @M2KEYWORDS@" << endl;
      template << "}" << endl << close;
      get template
      generateGrammar(template, outfile, x -> demark(",\n    ", x))
      get outfile
    Text
      If @SAMP "template"@ is omitted, then it defaults to
      @M2CODE "currentFileDirectory | outfile | \".in\""@.
///

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages PACKAGES=Style "
-- End:
