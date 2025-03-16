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
      support research in commutative algebra, algebraic geometry and related fields.
      All components of the language are open sourced, including over two hundred
      contributed packages, and generously funded by the National Science Foundation.
    Tree
      :Getting Started
	> "setting up Macaulay2"
	> "a first Macaulay2 session"
	> "reading the documentation"
	> "getting help or reporting bugs"
	-- TODO: "contributing to Macaulay2"
      :Mathematical Tutorials
	-- In this section we present some tutorials that aim to introduce
	-- the user to some mathematical ways of using Macaulay2.  The tutorials
	-- are relatively independent of each other, and each one introduces the use
	-- of some features of Macaulay2 in a slow and leisurely way, assuming the
	-- reader is already familiar with the mathematical concepts involved.
	-- -- @TO "David Eisenbud"@ joins us as a co-author of these tutorials.
	> "A first course in commutative algebra"
	> "Tutorial: Modules in Macaulay2"
	> "Tutorial: Elementary uses of Gröbner bases"
	> "Tutorial: Canonical Embeddings of Plane Curves and Gonality"
	> "Tutorial: Fano varieties"
	> "Tutorial: Divisors"
      :Mathematical Objects
        > "rings"
        > "ideals"
        > "matrices"
        > "modules"
        > @TO2(map, "morphisms")@
        > @TO2("Complexes :: Complexes", "chain complexes")@ (also see @TO2("chain complexes", "legacy version")@)
        > @TO2("Varieties :: Varieties", "varieties and sheaves")@
      :Mathematical Computations
        > "elementary arithmetic"
        > "commutative algebra"
        > "homological algebra"
        > "algebraic geometry"
        > "combinatorics"
        > "analytic functions"
      :Reference Manual
        > "The Macaulay2 language"
      :Packages
        > "packages provided with Macaulay2"
    Tree
      :Appendix
        > "changes to Macaulay2, by version"
       -- Mike wanted this: TO "frequently encountered problems"
        > "replacements for commands and scripts from Macaulay"
       -- TODO: node to be written, will point to our book, Hal's book, Mike's Arizona notes, Sage, etc:
        > "Other sources of information about Macaulay2"
  Acknowledgement
   Tree
    > "Acknowledgements"
    > "Copyright and license"
  Contributors
   Tree
    > "The authors of Macaulay2"
    > "The authors of Macaulay2 packages"
    - The contributors of Macaulay2 documentation:
      - Daniel Grayson
      - Michael Stillman
      - Mahrud Sayrafi
      - Doug Torrance
      - Paul Zinn-Justin
      - Lily Silverstein
      - Frédéric Chapoton
      - Zach Teitler
      - Anton Leykin
      - Guillem Blanco
      - Dave Barton
      - Brian Pike
      - Michael Burr
      - Frank Moore
      - Fatemeh Tarashi
      - Dylan Peifer
      - Thomas Kahle
      - Marc Harkonen
      - David Eisenbud
      - Boyana Martinova
      - Eliana Duarte
      - Lars Kastner
      - Kinsun Lee
  --References
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
