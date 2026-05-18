-- -*- coding: utf-8 -*-
doc ///
Node
  Key
    Macaulay2Doc
   "Macaulay2"
  Headline
    Macaulay2 documentation
  Description
    Code
      IMG ("src" => replace("PKG", "Style", currentLayout#"package") | "9planets.gif", "alt" => "image of nine planets")
    Text
      Macaulay2 is an interpreted, dynamically typed programming language designed to
      support research in commutative algebra, algebraic geometry, and related fields.
      This documentation includes getting-started guides, language and reference material,
      tutorials, and package documentation for the packages distributed with Macaulay2.
    Text
      If you are new to Macaulay2, start with @TO "setting up Macaulay2"@,
      @TO "a first Macaulay2 session"@, and @TO "reading the documentation"@.
      If you know the mathematical object or computation you want, use the
      Mathematical Objects and Mathematical Computations sections below to move
      from concepts such as rings, ideals, matrices, modules, Gröbner bases,
      Hilbert functions, and resolutions to the corresponding commands and examples.
      For package-level tools, start with @TO2("packages", "loading and using packages")@
      or @TO2("packages provided with Macaulay2", "browsing packages by subject area")@.
    Tree
      :Getting Started
	> "setting up Macaulay2"
	> "a first Macaulay2 session"
	> "reading the documentation"
	> "getting help or reporting bugs"
      :Practical Guides
        > "The Macaulay2 language"
        > @TO2("packages", "loading and using packages")@
        > @TO2("packages provided with Macaulay2", "browsing packages by subject area")@
        > "debugging"
        > "the debugger"
        > "error handling"
      :Mathematical Tutorials
	-- These tutorials begin with broader introductions and continue with
	-- more specialized worked examples in algebra and geometry.
	> "Tutorial: Modules in Macaulay2"
	> "Tutorial: Elementary uses of Gröbner bases"
	> "Tutorial: Divisors"
	> "Tutorial: Canonical Embeddings of Plane Curves and Gonality"
	> "Tutorial: Fano varieties"
	> "Tutorial: Numerical algebraic geometry"
	> "Teaching Materials"
      :Mathematical Objects
        > "rings"
        > "ideals"
        > "matrices"
        > "modules"
        > @TO2(map, "morphisms")@
        > @TO2("Complexes :: Complexes", "chain complexes")@ (also see @TO2("OldChainComplexes :: OldChainComplexes", "legacy version")@)
        > @TO2("Varieties :: Varieties", "varieties and sheaves")@
        > @TO2("Graphs :: Graphs", "graphs")@
        > @TO2("Posets :: Posets", "posets")@
        > @TO2("SimplicialComplexes :: SimplicialComplexes", "simplicial complexes")@
        > @TO2("Polyhedra :: Polyhedra", "polyhedra")@
      :Mathematical Computations
        > @TO2("elementary arithmetic", "Elementary Arithmetic")@
        > @TO2("linear algebra", "Linear Algebra")@
        > @TO2("commutative algebra", "Commutative Algebra")@
        > @TO2("homological algebra", "Homological Algebra")@
        > @TO2("algebraic geometry", "Algebraic Geometry")@
        > "Representation Theory"
        > @TO2("combinatorics", "Combinatorics")@
        > @TO2("analytic functions", "Analytic Functions")@
    Tree
      :Appendix
        > "changes to Macaulay2, by version"
       -- Mike wanted this: TO "frequently encountered problems"
        > "replacements for commands and scripts from Macaulay"
  Acknowledgement
   Tree
    > "Acknowledgements"
    > "The authors of Macaulay2"
    > "The authors of Macaulay2 packages"
    > "Copyright and license"
  Contributors
    Contributors to Macaulay2 documentation include Daniel Grayson, Michael Stillman,
    Mahrud Sayrafi, Doug Torrance, Paul Zinn-Justin, Lily Silverstein, Frédéric Chapoton,
    Zach Teitler, Anton Leykin, Guillem Blanco, Dave Barton, Brian Pike, Michael Burr,
    Frank Moore, Fatemeh Tarashi, Dylan Peifer, Thomas Kahle, Marc Harkonen, David Eisenbud,
    Boyana Martinova, Eliana Duarte, Lars Kastner, and Kisun Lee; additional names credited
    in documentation files outside git history include Sorin Popescu, Manoj Kummini,
    Leah Gold, Amelia Taylor, Giulio Caviglia, Gregory G. Smith, Wolfram Decker,
    Jonah Blasiak, Josephine Yu, and Irena Peeva.
  References
    -- TODO: node to be written, will point to our book, Hal's book, Mike's Arizona notes, Sage, etc:
    @UL {
	LI { HREF{"https://macaulay2.com/", "Macaulay2.com"}, " website, for online documentation, binary distributions, etc." },
	LI { SPAN "Computations in algebraic geometry with Macaulay2,
	    Algorithms and Computations in Mathematics (No. 8),
	    edited by David Eisenbud, Daniel R. Grayson, Michael E. Stillman, and Bernd Sturmfels,
	    Springer-Verlag, 2001, ISBN 3-540-42230-7." },
	LI { SPAN "Computational Algebraic Geometry,
	    London Mathematical Society Student Texts (No. 58),
	    by Hal Schenck,
	    Cambridge University Press, 2003
	    ISBN: 0-521-53650-2." }
    }@
///

end--

restart
--errorDepth=1
--debugLevel=1
elapsedTime loadPackage("Macaulay2Doc", LoadDocumentation => true, Reload => true)
elapsedTime installPackage(
    Macaulay2Doc,
    Verbose => true,
    RerunExamples => false,
    CheckDocumentation => true,
    IgnoreExampleErrors => false,
    RemakeAllDocumentation => false,
    InstallPrefix => "/home/mahrud/Projects/M2/quickfix/M2/BUILD/build/usr-dist/",
    UserMode => false,
    MakeInfo => false,
    SeparateExec => true,
    DebuggingMode => true)
