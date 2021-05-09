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
      Macaulay2 is a software system devoted to supporting research in algebraic geometry and commutative algebra,
      developed with funding from the National Science Foundation. We are eager to help new users get started with it.
  Subnodes
    :Preface
      "Copyright and license"
      "Acknowledgements"
      "The authors"
      -- TODO: node to be written, will point to our book, Hal's book, Mike's Arizona notes, Sage, etc:
      "Other sources of information about Macaulay2"
    :For the New User
      "getting started"
    :Mathematical Objects
      "rings"
      "ideals"
      "matrices"
      "substitution and maps between rings"
      "modules"
      "chain complexes"
      "varieties"
      "Gröbner bases"
    :Some Mathematical Computations
      "normal forms"
      -- Mike wanted this: TO "Hilbert functions"
      "elimination of variables"
      -- Mike wanted this: TO "syzygies"
      -- Mike wanted this: TO "saturation"
      -- Mike wanted this: TO "fibers of a map between varieties"
      -- Mike wanted this: TO "solving systems of polynomial equations"
      "IntegralClosure :: IntegralClosure"
      "PrimaryDecomposition :: primaryDecomposition"
      "combinatorial functions"
    :The Macaulay2 Language
      "The Macaulay2 language"
    :Packages
      "packages provided with Macaulay2"
      "authors of Macaulay2 packages"
    :Appendix
      "changes to Macaulay2, by version"
      "mathematical examples"
      "basic commutative algebra"
      -- Mike wanted this: TO "frequently encountered problems"
      "replacements for commands and scripts from Macaulay"
      "how Macaulay2 finds its files"
    :Reference material
      "Type"
      "Function"
///

document { Key => Core,
     Headline => "the core part of Macaulay2",
     PARA {
     	  "This package contains the core functionality of Macaulay2, without the documentation,
     	  which is in the package ", TO "Macaulay2Doc", "."
	  }
     }
