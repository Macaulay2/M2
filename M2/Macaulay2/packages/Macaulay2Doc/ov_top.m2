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
      This documentation includes getting-started guides, package and language documentation,
      mathematical reference material, tutorials, and developer-facing documentation.
    Text
      If you are new to Macaulay2, start with @TO "setting up Macaulay2"@,
      @TO "a first Macaulay2 session"@, and @TO "reading the documentation"@.
      Use the sections below to move from learning the system, to working with
      packages and the language, to guided mathematical examples and subject-area
      references, to developer and internal documentation. For longer walkthroughs,
      start with @TO2("Tutorials::Tutorials", "Tutorials")@.
    Tree
      > "Start Here"
      > "Using Macaulay2"
      > "Doing Mathematics"
      > "For Developers"
      > "About"
  Contributors
    Contributors to Macaulay2 documentation include Daniel Grayson, Michael Stillman,
    Mahrud Sayrafi, Doug Torrance, Paul Zinn-Justin, Lily Silverstein, Frédéric Chapoton,
    Zach Teitler, Anton Leykin, Guillem Blanco, Dave Barton, Brian Pike, Michael Burr,
    Frank Moore, Fatemeh Tarashi, Dylan Peifer, Thomas Kahle, Marc Harkonen, David Eisenbud,
    Boyana Martinova, Eliana Duarte, Lars Kastner, and Kisun Lee; additional names credited
    in documentation files outside git history include Sorin Popescu, Manoj Kummini,
    Leah Gold, Amelia Taylor, Giulio Caviglia, Gregory G. Smith, Wolfram Decker,
    Jonah Blasiak, Josephine Yu, Irena Peeva, and Joel Dodge.
  References
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

document {
     Key => "Start Here",
     "Start here if you are new to Macaulay2 or want the basic entry points for
     installing it, starting a session, navigating the documentation, and finding help.",
     Subnodes => {
	  TO "setting up Macaulay2",
	  TO "a first Macaulay2 session",
	  TO "reading the documentation",
	  TO "getting help or reporting bugs"
	  }
     }

document {
     Key => "Using Macaulay2",
     "This section routes you into the language, package workflow, and the most
     common practical topics for daily use.",
     PARA{},
     UL {
	  TO "The Macaulay2 language",
	  TO "packages",
	  TO "packages provided with Macaulay2",
	  TO "debugging",
	  TO "error handling"
	  },
     Subnodes => {
	  TO "The Macaulay2 language",
	  TO "packages provided with Macaulay2"
	  }
     }

document {
     Key => "Doing Mathematics",
     "This section groups the tutorial landing page together with the main
     subject-area overview pages for objects and computations.",
     Subnodes => {
	  TO "Tutorials and Examples",
	  TO "By object",
	  TO "By computation"
	  }
     }

document {
     Key => "Tutorials and Examples",
     "This page gathers the longer guided walkthroughs, teaching materials, and
     other tutorial-style entry points.",
     Subnodes => {
	  TO "Tutorials::Tutorials"
	  }
     }

document {
     Key => "By object",
     "These overview pages are organized by the kind of mathematical object you
     are working with.",
     Subnodes => {
	  TO "rings",
	  TO "ideals",
	  TO "matrices",
	  TO "modules",
	  TO "morphisms",
	  TO "Complexes :: Complexes",
	  TO "Varieties :: Varieties",
	  TO "Graphs :: Graphs",
	  TO "Posets :: Posets",
	  TO "SimplicialComplexes :: SimplicialComplexes",
	  TO "Polyhedra :: Polyhedra"
	  }
     }

document {
     Key => "By computation",
     "These overview pages are organized by the kind of computation you want to
     perform.",
     Subnodes => {
	  TO "elementary arithmetic",
	  TO "linear algebra",
	  TO "commutative algebra",
	  TO "homological algebra",
	  TO "algebraic geometry",
	  TO "Representation Theory",
	  TO "combinatorics",
	  TO "analytic functions"
	  }
     }

document {
     Key => "For Developers",
     "This section routes to package-authoring, documentation, debugging, and
     internal implementation material without changing the underlying ownership
     of those nodes.",
     PARA{},
     UL {
	  TO "creating a package",
	  TO "writing documentation",
	  TO "debugging",
	  TO "error handling",
	  TO "Macaulay2Doc::Core",
	  TO "how Macaulay2 finds its files",
	  TO "the interpreter of Macaulay2",
	  TO "the engine of Macaulay2",
	  TO "parallelism in engine computations"
	  }
     }

document {
     Key => "About",
     "This section collects acknowledgements, authorship, licensing, version
     history, and legacy material related to Macaulay2.",
     Subnodes => {
	  TO "Acknowledgements",
	  TO "The authors of Macaulay2",
	  TO "The authors of Macaulay2 packages",
	  TO "Copyright and license",
	  TO "changes to Macaulay2, by version",
	  TO "replacements for commands and scripts from Macaulay"
	  }
     }

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
