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
      Use the expanded sections below to move from learning the system, to working
      with packages and the language, to guided mathematical examples and subject-area
      references, to developer and internal documentation. For longer walkthroughs,
      start with @TO2("Tutorials::Tutorials", "Tutorials")@.
    Tree
      :Start Here
        > "setting up Macaulay2"
        > "a first Macaulay2 session"
        > "reading the documentation"
        > "getting help or reporting bugs"
      :Using Macaulay2
        > "The Macaulay2 language"
        * "lists and sequences"
        * "packages"
        > "packages provided with Macaulay2"
      :Doing Mathematics
        :Tutorials and Examples
          * "Tutorials::Tutorials"
        :By Area
          > "elementary arithmetic"
          > "linear algebra"
          > "commutative algebra"
          > "homological algebra"
          > "algebraic geometry"
          > "Representation Theory"
          > "combinatorics"
          > "analytic functions"
	:By Mathematical Object
          > "rings"
          > "ideals"
          > "matrices"
          > "modules"
          > "morphisms"
          > "Complexes :: Complexes"
          > "Varieties :: Varieties"
          > "Graphs :: Graphs"
          > "Posets :: Posets"
          > "SimplicialComplexes :: SimplicialComplexes"
          > "Polyhedra :: Polyhedra"
      :For Developers
        * "creating a package"
        * "writing documentation"
        * "debugging"
        * "error handling"
        * "Macaulay2Doc::Core"
        * "how Macaulay2 finds its files"
        * "the interpreter of Macaulay2"
        * "the engine of Macaulay2"
        * "parallelism in engine computations"
      :About
        > "Acknowledgements"
        > "The authors of Macaulay2"
        > "The authors of Macaulay2 packages"
        > "Copyright and license"
        > "changes to Macaulay2, by version"
        > "replacements for commands and scripts from Macaulay"
        :Contributors
          :Contributors to Macaulay2 documentation include Daniel Grayson, Michael Stillman, Mahrud Sayrafi, Doug Torrance, Paul Zinn-Justin, Lily Silverstein, Frédéric Chapoton, Zach Teitler, Anton Leykin, Guillem Blanco, Dave Barton, Brian Pike, Michael Burr, Frank Moore, Fatemeh Tarashi, Dylan Peifer, Thomas Kahle, Marc Harkonen, David Eisenbud, Boyana Martinova, Eliana Duarte, Lars Kastner, and Kisun Lee; additional names credited in documentation files outside git history include Sorin Popescu, Manoj Kummini, Leah Gold, Amelia Taylor, Giulio Caviglia, Gregory G. Smith, Wolfram Decker, Jonah Blasiak, Josephine Yu, Irena Peeva, and Joel Dodge.
        :References
          :@HREF{"https://macaulay2.com/", "Macaulay2.com"}@ website, for online documentation, binary distributions, etc.
          :Computations in algebraic geometry with Macaulay2, Algorithms and Computations in Mathematics (No. 8), edited by David Eisenbud, Daniel R. Grayson, Michael E. Stillman, and Bernd Sturmfels, Springer-Verlag, 2001, ISBN 3-540-42230-7.
          :Computational Algebraic Geometry, London Mathematical Society Student Texts (No. 58), by Hal Schenck, Cambridge University Press, 2003, ISBN 0-521-53650-2.
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
