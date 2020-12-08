-- see functions/package-doc.m2 for all package related functions

-- subnodes of "Macaulay2"
doc ///
Node
  Key
    "packages provided with Macaulay2"
  Description
    Text
      Here is a list of the packages that are distributed with Macaulay2.
      The ones that have been refereed are marked with a star.
    Code
      star := IMG {"src" => replace("PKG", "Style", currentLayout#"package") | "GoldStar.png", "alt" => "a gold star"};
      categories := new MutableHashTable from {};
      pkgs := separate_" " version#"packages";
      oops := select(tally pkgs, v -> v > 1); -- are any packages listed twice?  If so, it will cause an inscrutable error below when sorting pairs
      if #oops > 0 then error ("some packages listed more than once in `version#\"packages\"` : ", concatenate demark_"," keys oops);
      scan(pkgs, pkgname -> (
	      pkgopts := readPackage pkgname;
	      -- populate categories
	      scan(pkgopts.Keywords, keyword -> if not categories#?keyword
		  then categories#keyword = new MutableList from {(pkgname, pkgopts)}
		  else categories#keyword#(#categories#keyword) = (pkgname, pkgopts));
	      ));
      DIV flatten apply(sort keys categories, keyword -> {
              SUBSECTION {keyword},
              UL apply(sort new List from categories#keyword, pkgpair -> (
                      (pkgname, pkgopts) := pkgpair;
                      LI {
			  TO (pkgname | "::" | pkgname),
			  if pkgopts.Certification =!= null then (" ", star),
			  if pkgopts.Headline      =!= null then (" -- ", pkgopts.Headline)
			  }
                      ))
              })
  SeeAlso
    "packages"
    "authors of Macaulay2 packages"

Node
  Key
    "authors of Macaulay2 packages"
  Description
    Text
      The following people have authored packages that are distributed with Macaulay2:
    Code
      authors := new MutableHashTable from {};
      scan(separate_" " version#"packages", pkgname -> (
	      pkgopts := readPackage pkgname;
	      -- populate authors
	      scan(pkgopts.Authors, author -> (
		      author = new OptionTable from author;
		      name := if author.?Name then author.Name else return;
		      if match({"(C|c)ontribut", "(M|m)aintain", "(A|a)uthor", "(T|t)hank"}, name) then return;
		      if match("(Michael|Mike) Stillman", name) then name = "Michael E. Stillman";
		      if authors#?name
		      -- TODO: make append work with MutableList, then simplify this
		      then authors#name#"packages"#(#authors#name#"packages") = pkgname
		      else authors#name = new MutableHashTable from { "packages" => new MutableList from {pkgname}, "href" => "#" };
		      authors#name#"href" = if authors#name#"href" =!= "#" then authors#name#"href"
		      else if author.?HomePage then             author.HomePage
		      else if author.?Email    then "mailto:" | author.Email));
	      ));
      -- TODO: simplify this when sort takes a SortStrategy
      c := 0;
      htmlLiteral := value Core#"private dictionary"#"htmlLiteral";
      PARA{between_", \n" (last \ sort apply(pairs authors, (name, entry) -> (
		      link := ANCHOR{htmlLiteral name,
			  "href"  => htmlLiteral entry#"href",
			  "title" => concatenate("Packages: ", demark_", " new List from entry#"packages")};
		      (last separate(" ", name), c = c + 1, link)))), "."}
  SeeAlso
    "creating a package"
    "packages provided with Macaulay2"
///

-- subnodes of "The Macaulay2 language"
doc ///
Node
  Key
    "packages"
    "using packages" -- deprecate this
  Headline
    Macaulay2 packages
  Description
    Text
      A package is a body of Macaulay2 source code devoted to a particular topic.
      Many packages are distributed with Macaulay2, and others are available from
      the author's homepage.

      To load a package, say @TT "FirstPackage"@, enter:
    Pre
      loadPackage "FirstPackage"
    Text
      or
    Pre
      needsPackage "FirstPackage"
    Text
      For technical information about packages, see @TO Package@.

      Macaulay2 searches for the file @TT "FirstPackage.m2"@ on your search @TO "path"@.
      The packages provided with Macaulay2 are on your search path, as is your current
      working directory.
    Tree
      :Using existing packages
        loadPackage
        needsPackage
        dismiss
        importFrom
        "loadedPackages"
      :Creating a new package
        "creating a package"
        newPackage
        installPackage
        uninstallPackage
      :Writing documentation for a package
        "writing documentation"
        "SimpleDoc :: doc"
      :Debugging a package
        "the debugger"
        (debug, Package)
        (check, Package)
    Text
      Documentation for the packages provided with Macaulay2 is already installed.
      To install documentation for another package, use @TO installPackage@.
    Pre
      installPackage FirstPackage
    Text
      You may see what packages have been loaded with the variable @TO "loadedPackages"@.
  SeeAlso
    "creating a package"
    "packages provided with Macaulay2"
  Subnodes
    Package
    :Accessing packages
    "loadedPackages"
    package
    loadPackage
    readPackage
    needsPackage
    installPackage
    installedPackages
    uninstallPackage
    uninstallAllPackages
    :Interacting with packages
    importFrom
    (use, Package)
    (check, Package)
    (debug, Package)
    (dismiss, Package)
    (options, Package)
    :Miscellaneous nodes
    makePackageIndex
    "PackageDictionary"

Node
  Key
    "creating a package"
    "informing others about your package" -- deprecate this
  Description
    Text
      Creating a package is the most common way of contributing to Macaulay2. Packages can contain
      code for working with objects of a certain category, generating examples for testing a conjecture,
      or an implemention of algorithms introduced in the literature.

    Tree
      :There are five parts to a Macaulay2 package
        :the preamble, initiated by @TO newPackage@
        :a list of exported functions and variables, set via @TO export@ and @TO exportMutable@
        :the code that constitutes the package
        :the documentation for the package, which comes after @TO beginDocumentation@
        :a number of tests for the new package, added using @TO TEST@

    Text
      See @TO "an example of a package"@ for the basic template for new packages, or
      @TO "SimpleDoc :: packageTemplate"@ for a utility for generating the skeleton for a new package.

      The name of the package must be the name of the file, without the @TT ".m2"@ suffix.
      Thus a package @TT "NewMath"@ will be in a file named @TT "NewMath.m2"@.
      Other sources required by the package may be stored in a subdirectory named @TT "NewMath"@
      located in the same directory as the package.
    Text
      @HEADER3 "Documenting and testing a package"@

      The documentation and tests included in a package inform others about how to use your package
      and ensure that your package continues to work with future versions of Macaulay2.
      See @TO "writing documentation"@ for more information.
    Text
      @HEADER3 "Distributing a package"@

      To add your package to the list of packages distributed with Macaulay2, authors can submit their package via a
      @HREF {"https://github.com/Macaulay2/M2/pulls", "pull request to the GitHub repository for Macaulay2"}@.

      Before submitting your package, use the @TO installPackage@ and @TO check@ functions to ensure
      there are no errors in the package documentation and that all the tests pass.
  SeeAlso
    "packages"
    "writing documentation"
  Subnodes
    "an example of a package"
    "currentPackage"
    :Components of a package
    newPackage
    export
    exportFrom
    exportMutable
    beginDocumentation
    TEST
    endPackage

Node
  Key
    "loadedPackages"
  Headline
    the list of loaded packages
  Description
    Text
      The value of the variable @TT "loadedPackages"@ is a list of the packages that have been loaded.
    Example
      loadedPackages
  SeeAlso
    "packages provided with Macaulay2"
    dismiss
    needsPackage

Node
  Key
    "an example of a package"
  Description
    Text
      Here is a basic example of a complete package:
    Code
      EXAMPLE { PRE get (first searchPath(path, "FirstPackage.m2") | "FirstPackage.m2") }
    Text
      Also see @TO "SimpleDoc :: packageTemplate"@, a utility for generating the skeleton for a new package.
  SeeAlso
    "packages"
    newPackage
    export
    exportMutable
    beginDocumentation
    "SimpleDoc :: doc"
    TEST
///

--     Subnodes
--	  TO "using packages",				    -- ?
--	  TO "writing packages",			    -- ?
--	  TO "blow ups",
--	  TO "convex hulls and polar cones",
--	  TO "Dmodules",
--	  TO "elimination theory",
--	  TO "graphing curves and surfaces via 'surf'",
--	  TO "invariants of finite groups",
--	  TO "Lenstra-Lenstra-Lovasz (LLL) lattice basis reduction",
--	  TO "SAGBI bases",
