-- -*- coding: utf-8 -*-
--		Copyright 1993-2009 by Daniel R. Grayson


document {
     Key => "COPYING-GPL-2",
     Headline => "GNU General Public License, version 2",
     if prefixDirectory =!= null
     then PRE get(prefixDirectory | currentLayout#"docdir" | "COPYING-GPL-2")
     else (
	  stderr << "warning: can't locate file \"COPYING-GPL-2\"" << endl;
	  SPAN {"See the GNU GENERAL PUBLIC LICENSE, Version 2, June 1991, available at ", HREF "http://www.gnu.org/licenses/gpl-2.0.txt", "."}
	  )
     }

document {
     Key => "COPYING-GPL-3",
     Headline => "GNU General Public License, version 3",
     if prefixDirectory =!= null
     then PRE get(prefixDirectory | currentLayout#"docdir" | "COPYING-GPL-3")
     else (
	  stderr << "warning: can't locate file \"COPYING-GPL-3\"" << endl;
	  SPAN {"See the GNU GENERAL PUBLIC LICENSE, Version 3, 29 June 2007, available at ", HREF "http://www.gnu.org/licenses/gpl-3.0.txt", "."}
	  )
     }

document {
     Key => "Copyright and license",
     PARA {
	  "Macaulay2, its object code and source code, and its documentation,
	  are copyright by Daniel R. Grayson and Michael E. Stillman.  We permit 
	  you to use it either:
	  under the terms of the GNU General Public License, version 2, as published by the Free Software Foundation, and as
	  contained in the file ", TO "COPYING-GPL-2", " accompanying the program;
	  or:
	  under the terms of the GNU General Public License, version 3, as published by the Free Software Foundation, and as
	  contained in the file ", TO "COPYING-GPL-3", " accompanying the program."
	  },
     PARA {
	  "Various packages are included with Macaulay2 and come with their own copyright notices and licenses, see
	  ", TO "packages provided with Macaulay2", "."
	  },
     PARA {
      	  "Some free libraries have been compiled into (or linked with) Macaulay2, and some free programs, or packages
	  of programs, with their libraries, have been compiled and distributed with Macaulay2:"
	  },
     Subnodes => {
	  "libraries",
	  TO "Singular-Factory",
	  TO "frobby",
	  TO "pari",
	  TO "MPIR",
	  TO "GNU MP",
	  TO "MPFR",
	  TO "GC garbage collector",
	  TO "LAPACK",
	  TO "BLAS",
	  TO "NTL library",
	  TO "FLINT",
	  TO "mpack",
	  "programs and their libraries",
	  TO "4ti2",
	  TO "gfan",
	  TO "normaliz",
	  TO "nauty",
	  TO "cdd+",
	  TO "lrslib"
     	  }
     }

document {
     Key => "FLINT",
     PARA {
	  "The library ", TT "FLINT", " (Fast Library for Number Theory) is a
	  library for computations in number theory, consisting mainly of routines for
	  integer and polynomial arithmetic and linear algebra.  It was written
	  by William Hart, Mike Hansen, Sebastian Pancratz, Fredrik Johansson,
	  and others, and is available at ", HREF "http://flintlib.org/", ".  It is 
	  distributed under the terms of the GNU General Public License, version 2 or later."
	  }
     }

document {
     Key => "mpack",
     PARA {
	  "The library ", TT "mpack", " is a multiprecision linear algebra package based on lapack and blas,
	  written by Nakata Maho.  It is available at ", HREF "http://mplapack.sourceforge.net/", "."
	  }
     }

document {
     Key => "lrslib",
     PARA {
     	  "The program ", TT "lrslib", ", written by David Avis,
	  provides the reverse search algorithm for  vertex enumeration and convex hull problems.
	  It is available at ", HREF "http://www-cgrl.cs.mcgill.ca/~avis/C/lrs.html", ",
	  under the terms of the GNU General Public License, version 2."
	  }
     }

document {
     Key => "cdd+",
     PARA {
     	  "The program ", TT "cdd+", TEX ", written by Komei Fukuda,
	  is a C++ implementation of the double description 
	  method of Motzkin, et al., for generating all vertices
	  and extreme rays of a general convex polyhedron in $\\RR^d$ given by a system 
	  of linear inequalities.  It is available at ", HREF "http://www.ifor.math.ethz.ch/~fukuda/cdd_home/", ",
	  under the terms of the GNU General Public License, version 2."
	  }
     }

document {
     Key => "nauty",
     PARA {
     	  "The collection of programs called ", TT "nauty", ", written by Brendan McKay,
	  computes automorphism groups of graphs and digraphs.
	  It is available at ", HREF "http://cs.anu.edu.au/~bdm/nauty", ".  The package ", TO "Nauty::Nauty", " is
	  an interface to it."
	  },
     PARA {
	  "It is distributed with the following copyright notice: 
	  Copyright (1984-2010) Brendan McKay.  All rights reserved.  Permission is hereby given for use and/or distribution
	  with the exception of sale for profit or application with nontrivial military significance.  You must not remove
	  this copyright notice, and you must document any changes that you make to this program.  This software is subject
	  to this copyright only, irrespective of any copyright attached to any package of which this is a part.
	  Absolutely no guarantees or warranties are made concerning the suitability, correctness, or any other aspect of
	  this program.  Any use is at your own risk."
	  }
     }

document {
     Key => "normaliz",
     PARA {
     	  "The program ", TT "normaliz", ", written by Winfried Bruns, Bogdan Ichim, and Christof Soeger,
	  provides computations in affine monoids, vector configurations, lattice polytopes, 
     	  and rational cones.  It is available at ", HREF "http://www.mathematik.uni-osnabrueck.de/normaliz/", ".  The package
     	  ", TO "Normaliz::Normaliz", " is an interface to it.  It is licensed under the terms of
	  the GNU General Public License, version 3."
	  }
     }

document {
     Key => "gfan",
     "The program ", TT "gfan", ", written by Anders Jensen, computes Groebner fans.
     It is available at ", HREF "http://www.math.tu-berlin.de/~jensen/software/gfan/gfan0.4plus.tar.gz", ".
     The packages ", TO "StatePolytope::StatePolytope", " and ", TO "gfanInterface::gfanInterface", "
     run it."
     }

document {
     Key => "LAPACK",
     PARA {
	  "The ", TT "LAPACK", " library is used by Macaulay2 for its linear algebra routines.  It is
	  available at ", HREF "http://www.netlib.org/lapack/", "."
	  }
     }

document {
     Key => "BLAS",
     PARA {
	  "The ", TT "BLAS", " library is used by Macaulay2 for its linear algebra routines.  It is
	  available at ", HREF "http://www.netlib.org/blas/", "."
	  }
     }

document {
     Key => "NTL library",
     PARA {
	  "The ", TT "NTL", " library, written by Victor Shoup, is used by Macaulay2, through the ", TO "Singular-Factory", " library,
	  for its algebraic routines.  It is available at ", HREF "http://shoup.net/ntl/", "
	  and is licensed under the GNU Lesser General Public License, version 2 or later."
	  }
     }

document {
     Key => "MPFR",
     PARA {
	  "The ", TT "MPFR", " library is used by Macaulay2 for its arbitrary precision real number arithmetic
	  and associated transcendental functions.  It is available at ", HREF "http://www.mpfr.org/", ".  The
	  library is remarkable for the care taken to return correctly rounded
	  results.  It is hoped that this will form a good base for experimentation
	  with algebraic algorithms that mix symbolic and numeric techniques.
	  It is licensed under the GNU Lesser General Public License, version 3 or later."
	  }
     }

document {
     Key => "GC garbage collector",
     PARA {
	  "Macaulay2 uses the excellent garbage collector GC, version ", version#"gc version", ",
	  written by Hans-J. Boehm and Alan J. Demers and generously licensed
	  to the public.  It is available at
	  ", HREF "http://www.hpl.hp.com/personal/Hans_Boehm/gc/", "."
	  },
     PARA{
	  "Some environment variables can be set by the user to tune garbage collector performance:"
	  },
     UL {
	  LI { "GC_INITIAL_HEAP_SIZE -- initial heap size in bytes" },
	  LI { "GC_MAXIMUM_HEAP_SIZE -- maximum collected heap size" },
	  LI { "GC_FREE_SPACE_DIVISOR -- if set to a number D, then
                         we try to make sure that we allocate at least N/D bytes between collections, where N is twice the
                         number of traced bytes, plus the number of untraced bytes, plus a rough estimate of the root set
                         size.  Increasing its value will use less space but more collection time.  Decreasing it will
                         appreciably decrease collection time at the expense of space.
			 Macaulay2 sets the initial default value to 12." },
	  LI { "GC_PRINT_STATS -- whether to turn on logging" },
	  LI { "GC_PRINT_VERBOSE_STATS -- whether to turn on even more logging" },
	  LI { "GC_DUMP_REGULARLY -- whether to generate a debugging dump on startup and during every collection; very verbose" },
	  LI { "GC_NPROCS -- the number of processors to use (for Linux only)" },
	  LI { "GC_MARKERS -- the number
                of marker threads.  This is normally set to the number of
                processors.  It is safer to adjust GC_MARKERS than GC_NPROCS,
                since GC_MARKERS has no impact on the lock implementation" }
	  },
     PARA{
	  "The full list is found in the source code for gc in the file ", TT "doc/README.environment", "."
	  },
     PARA {
	  "Here are some error messages you may see from it when it aborts the program, due to lack of memory or related problems.
	  Typically, the only recourse for the user is to increase the memory available to the program."
	  },
     UL {
	  "Insufficient space for initial table allocation",
	  "No space for lwp data structures",
	  "Out of memory",
	  "Too many exclusions",
	  "Too many heap sections",
	  "Too many heap sections: Increase MAXHINCR or MAX_HEAP_SECTS",
	  "Too many root sets"
	  },
     SeeAlso => { collectGarbage }
     }

document {
     Key => collectGarbage,
     Headline => "collect the garbage in memory",
     Usage => "collectGarbage()",
     Consequences => {
	  {"garbage is collected"}
	  },
     SeeAlso => "GC garbage collector"
     }

document {
     Key => "pari",
     PARA {
	  "Starting with version 1.2, Macaulay2 incorporates ", TT "pari", ", a free library for fast computations in number theory,
	  available at ", HREF "http://pari.math.u-bordeaux.fr/", ",
	  originally developed by Henri Cohen and his co-workers at Université Bordeaux I, France.
	  It is used by ", TO (factor,ZZ), ", ", TO (factor,QQ), ", ", TO (isPseudoprime, ZZ), ", and ", TO (isPrime,ZZ), ".
	  It is distributed under the terms of the GNU General Public License, version 2, see ", TO "COPYING-GPL-2", "."
	  }
     }

document {
     Key => "frobby",
     PARA {
	  "Starting with version 1.2, Macaulay2 incorporates ", TT "frobby", ", a free library of routines for computing
	  the Alexander dual of a monomial ideal (see ", TO (dual,MonomialIdeal), ", a method used internally
	       by many routines), written
	  by Bjarke Hammersholt Roune, and available at ", HREF "http://www.broune.com/frobby/", ".
	  It is distributed under the terms of the GNU General Public License, version 2 (or later), see ", TO "COPYING-GPL-2", "."
	  }
     }

document {
     Key => "Singular-Factory",
     "With the kind permission of the authors of Singular, 
     G.-M. Greuel, R. Stobbe, G. Pfister, H. Schoenemann, and J. Schmidt,
     University of Kaiserslautern, Macaulay2 incorporates ", TT "Singular-Factory", ",
     version ", version#"factory version", ", a free library of polynomial routines
     that provides for factorization of polynomials.  It is distributed under the
     terms of the GNU General Public License (version 2 (see ", TO "COPYING-GPL-2", ")
	  or version 3 (see ", TO "COPYING-GPL-3", ")) and is available at 
     ", HREF "ftp://www.mathematik.uni-kl.de/pub/Math/Singular/Factory"," and at 
     ", HREF "http://www.mathematik.uni-kl.de/ftp/pub/Math/Singular/SOURCES", ".  It
     is part of ", TT "Singular", ", whose home page is ", HREF "http://www.singular.uni-kl.de/", ".",
     PARA{},
     "The following important routines call upon it:",
     UL {
	  TOH "factor",
	  TOH "gcd",
	  TOH "minimalPrimes",
	  TOH "irreducibleCharacteristicSeries"
	  }
     }

document {
     Key => "MPIR",
     "The MPIR library, version ", version#"mpir version", " provides
     routines for arbitrary precision integer and floating point arithmetic,
     and is distributed under the terms of the GNU Lesser General Public License (LGPL), version 2.1 or later.
     It is available at ", HREF "http://www.mpir.org/", ".  To see whether your copy of Macaulay2 is linked
     with it (or with GMP), examine the variable ", TO "version", "."
     }

document {
     Key => "4ti2",
     "The package of programs ", TT "4ti2", " is dedicated to algebraic, geometric and combinatorial
     problems on linear spaces, and is distributed under the terms of the GNU General Public License (GPL), version 2 or later.
     It is available at ", HREF "http://www.4ti2.de/", ".  The package ", TO "FourTiTwo::FourTiTwo", " runs it."
     }

document {
     Key => "GNU MP",
     "The GNU MP library (GMP) provides
     routines for arbitrary precision integer and floating point arithmetic,
     and is distributed under the terms of the GNU Lesser General Public License (LGPL), version 3,
     and also under the GNU General Public License (GPL), version 2.
     It is available at ", HREF "ftp://ftp.gnu.org/gnu/gmp/", " and ", HREF "http://gmplib.org/", ".  To see whether your copy of Macaulay2 is linked
     with it (or with MPIR), examine the variable ", TO "version", "."
     }

document {
     Key => "Acknowledgements",
     "We thank the National Science Foundation for generous funding since
     1993 for this project, Gert-Martin Greuel and Ruediger Stobbe for the
     incorporation of their library ", TO "Singular-Factory", ",
     and David Eisenbud, Wolfram Decker and Sorin Popescu for
     early support, encouragement and suggestions.  We also acknowledge an
     intellectual debt to Dave Bayer, who, with Michael Stillman,
     wrote Macaulay, a specialized computer algebra system for algebraic
     geometry and the predecessor of this program."
     }

{* -- Mike wanted this: 
document {
     Key => "preface",
     }
*}

document {
     Key => "prefixPath",
     Headline => "absolute locations of Macaulay2 files",
     PARA {
	  "The absolute location of a Macaulay2 file can be obtained by concatenating three components: (a) the
	  prefix, which is one of the members of the list ", TO "prefixPath", "; (b) the relative location of the directory
	  containing the file, as recorded in the hash table ", TO "Layout", "; and (c) the base name of the file.
	  The value of ", TO "prefixPath", " is used by ", TO "installPackage", " when determining how to direct
	  documentation hyperlinks from one package to another, provided the option ", TO "AbsoluteLinks", " is set to ", TO "true", "."
	  },
     PARA {
	  "The initial value of ", TO "prefixPath", " contains just the following two optional items.
	  If the variable ", TO "prefixDirectory", " was
	  given a non-null value initially or by a ", TT "-e", " command line argument,
	  then it will be the last element of ", TO "prefixPath", ".  If the ", TT "-q", " 
	  option was not given on the command line used to invoke Macaulay2, then the value of ", TT "applicationDirectory()|\"local/\"", "
	  will be the first element of ", TO "prefixPath", ".  No attempt is made to synchronize the value of ", TO "prefixPath", "
	  with the values of ", TO "prefixDirectory", " and of ", TT "applicationDirectory()", ", which may change."
	  },
     PARA {
	  "When running a newly compiled version of Macaulay2, adding something like ", TT "-E 'prefixDirectory=\"/usr/\"'", " to
	  the command line is a good way to direct hyperlinks created by ", TO "installPackage", " to the documentation provided by
	  an older copy of Macaulay2 installed with the prefix ", TT "/usr/", ", and that, in turn, is easily done within
	  emacs by the keystroke sequence ", TT "CTRL-U f12", ", which offers you a chance to edit the command line."
	  },
     PARA {
	  "The initial value of ", TO "prefixPath", " described above can be overridden by the user's ", TO "initialization file", ")."
	  },
     PARA {
	  "The list ", TO "prefixPath", " should be distinguished from the list ", TO "path", ", which is used to locate files to be
	  loaded, by functions such as ", TO "searchPath", ", ", TO "load", ", ", TO "loadPackage", ", and ", TO "needsPackage", "."
	  },
     PARA {
	  "The following example shows the list of places where we might find the source code of a package called ", TT "Foo", "
	  after it has been installed by ", TO "installPackage", "."
	  },
     EXAMPLE ///stack apply(prefixPath, p -> p | Layout#1#"packages" | "Foo.m2")///,
     PARA {
     	  "This example shows the list of places where we might reasonably find the html file documenting a
	  function named ", TT "bar", " in a package called ", TT "Foo", "."
	  },
     EXAMPLE ///stack apply(prefixPath, p -> p | replace("PKG","Foo",Layout#1#"packagehtml") | "bar.html")///,
     PARA {
     	  "This example shows the list of places where we might reasonably find the info file documenting a
	  package called ", TT "Foo", "."
	  },
     EXAMPLE ///stack apply(prefixPath, p -> p | Layout#1#"info" | "Foo.info")///,
     SeeAlso => {"commandLine", "Invoking the program", applicationDirectory, "prefixDirectory", "path", searchPath, load, loadPackage, needsPackage}
     }

doc := new HashTable from {
     "bin" => "executable files (M2)",
     "common" => "architecture independent files",
     "data" => "architecture independent data files",
     "doc" => "documentation",
     "docdir" => "documentation for Macaulay2 packages",
     "emacs" => "emacs source files (*.el, *.elc)",
     "exec" => "architecture dependent files",
     "factory gftables" => "directory for files containing addition tables in small finite fields used by the library 'factory'",
     "info" => "documentation in info form",
     "lib" => "architecture dependent data and executable files",
     "libraries" => "dynamically loadable libraries from third party packages linked with Macaulay2",
     "man" => "man pages",
     "package" => "additional source files for the Macaulay2 package PKG",
     "packagecache" => "cached data files for the Macaulay2 package PKG",
     "packagedoc" => "documentation for the Macaulay2 package PKG",
     "packageexampleoutput" => "example output files for the Macaulay2 package PKG",
     "packagehtml" => "html documentation for the Macaulay2 package PKG (*.html)",
     "packageimages" => "images for the Macaulay2 package PKG (*.jpg)",
     "packagelib" => "architecture dependent files for the Macaulay2 package PKG",
     "packages" => "source files for Macaulay2 packages; this directory appears on the path",
     "packagetests" => "test files for the Macaulay2 package PKG",
     "programs" => "programs to be run by Macaulay2",
     "program licenses" => "licenses for programs to be run by Macaulay2"
     }
assert( set keys Layout#1 === set keys Layout#2 )
assert( set keys Layout#1 === set keys doc )

document {
     Key => {"currentLayout", "Layout"},
     Headline => "relative locations of Macaulay2 files",
     PARA {
	  "Macaulay2 comes with a variety of types of files, and some of them are associated with a 
	  particular Macaulay2 package.  The hash table ", TT "currentLayout", " is a translation 
	  table from names, corresponding to the various types of files, to directory paths.  The
	  directory paths are to be interpreted relative to the path stored in ", TO "prefixDirectory", " or in one
	  of the directories contained in the list ", TO "prefixPath", ".
	  Some of the strings contain ", TT "PKG", " as a substring, which should be replaced
	  by the name of package whose files will be stored in that directory."
	  },
     PARA {
	  "The hash table ", TO "Layout", " contains the two possible values for ", TO "currentLayout", ";
	  corresponding to the two possible values for the ", TO "SeparateExec", " option used with ", TO "installPackage", ".
	  The hash table ", TT "Layout#2", " is used if architecture dependent files are to be stored in
	  a directory tree separate from the one used for architecture independent files.  The hash table ", TT "Layout#1", "
	  is used otherwise."
	  },
     PARA {
	  "Basic Macaulay2 files are regarded as being associated
	  with a special package called ", TO2{"Macaulay2Doc::Core", "Core"}, ", and the corresponding documentation files
	  are part of the package ", TT "Macaulay2Doc", "."
     	  },
     EXAMPLE {
	  "Layout"
	  },
     "Here are the meanings of the keys used in ", TO "currentLayout", ".",
     UL apply(sort pairs doc, (k,v) -> LI { TT format k, " : " | v}),
     SeeAlso => {[installPackage,SeparateExec]}
     }

document {
     Key => "mathematical examples",
     "In this section we present some tutorials that aim to introduce
     the user to some mathematical ways of using Macaulay2.  The tutorials
     are relatively independent of each other, and each one introduces the use
     of some features of Macaulay2 in a slow and leisurely way, assuming the
     reader is already familiar with the mathematical concepts involved.  
     ", TO "David Eisenbud", " joins us as a co-author of these tutorials.",
     Subnodes => {
	  TO "Tutorial: Elementary uses of Groebner bases",
	  TO "Tutorial: Canonical Embeddings of Plane Curves and Gonality",
	  TO "Tutorial: Fano varieties",
	  TO "Tutorial: Divisors",
	  }
     }

document {
     Key => "basic commutative algebra",
     "This section includes tutorials showing how to do
     basic commutative algebra constructions in Macaulay2.
     This section is being written by Mike Stillman, for use
     with his Fall 2005 course: Math 634, Commutative algebra,
     at Cornell University.  This course covers basic commutative
     algebra, at the level of Atiyah-Macdonald, and Greuel-Pfister.",
     PARA{},
     "Macaulay2 examples corresponding to the Singular examples in the
     book by Greuel-Pfister may also be found here.",
     Subnodes => {
	  TO "Elementary uses of Groebner bases I. Math 634 Fall 2005",
	  TO "modules in Macaulay2",
	  TO "M2SingularBook"
	  }
     }

document {
     Key =>  "Developer's Corner",
     Subnodes => {
	  TO "engine",
	  }
     }

{* -- Mike wanted this: 
document { Key => "frequently encountered problems",
     }
*}

document {
     Key => "specifying typical values",
     "For the purpose of construction good documentation automatically, it
     is useful to specify the type of value typically returned by a function
     or method.  For example, the function ", TO "isModule", " returns a boolean
     value, and this is specified when creating the method function with the
     option ", TO "TypicalValue", " as follows.",
     PRE ///isModule = method(TypicalValue => Boolean)///,
     PARA{},
     "Other functions, such as ", TO "prune", ", return values of various types,
     depending on the type of the arguments provided.  To install a
     function ", TT "f", " as the handler for ", TT "prune", " applied to a matrix,
     we would normally use the following statement.",
     PRE ///prune Matrix := f///,
     "To specify that the value typically returned is a matrix (of class ", TT "Matrix", "),
     we replace ", TT "f", " by ", TT "Matrix => f", ", as follows.",
     PRE ///prune Matrix := Matrix => f///,
     "Here is the way our code looks.",
     EXAMPLE "code(prune, Matrix)",
     "The information is stored in the hash table ", TO "typicalValues", ", and can
     be recovered like this.",
     EXAMPLE "typicalValues#(prune,Matrix)",
     PARA{},
     "Warning: don't imagine that a definition of the form ",
     PRE "f = t -> (...)",
     "can be replaced with a declaration of the following form.",
     PRE "f = X => t -> (...)",
     "The difference here is that here we are using simple assignment, rather than
     installing a method.  To document the return type is ", TT "X", " in this case, 
     make an entry in ", TT "typicalValues", " directly.",
     PRE "f = t -> (...)\ntypicalValues#f = X"
     }

document {
     Key => "The authors",
     PARA{},
     "To communicate with the authors about the program, use this email address:
     ", HREF {"mailto:Macaulay2@math.uiuc.edu", "<Macaulay2@math.uiuc.edu>"}, ".",
     Subnodes => {
     	  "The authors of Macaulay2:",
	  TO "Daniel R. Grayson",
	  TO "Michael E. Stillman",
     	  "Our co-author for the tutorials, long time supporter and user of Macaulay2, and collaborator on the project since 2007:",
	  TO "David Eisenbud",
	  }
     }

document {
     Key => "David Eisenbud",
     HREF {"http://www.msri.org/~de/", "David Eisenbud "}, " -- ", HREF {"mailto:de@msri.org", "<de@msri.org>"}, ".",
     PARA{
	  "David Eisenbud received his PhD in mathematics in 1970 at the University of
	  Chicago under Saunders MacLane and Chris Robson, and was on the faculty at
	  Brandeis University before coming to Berkeley, where he has been Professor of
	  Mathematics since 1997. He has been a visiting professor at Harvard, Bonn, and
	  Paris. Eisenbud's mathematical interests range over commutative and
	  non-commutative algebra, algebraic geometry, topology, and computer methods."
	  },
     PARA{
	  "Eisenbud served as Director of MSRI from 1997 to 2007. He was President of the
	  American Mathematical Society from 2003 to 2005. He is a Director of Math for
	  America, a foundation devoted to improving mathematics teaching. He has been a
	  member of the Board of Mathematical Sciences and their Applications of the
	  National Research Council, and is a member of the US National Committee of the
	  International Mathematical Union, which he will chair, starting in 2010. In
	  2006 Eisenbud was elected a Fellow of the American Academy of Arts and
	  Sciences. Eisenbud is Chair of the Editorial Board of the Algebra and Number
	  Theory journal, which he helped found in 2006, and serves on the editorial
	  boards of the Bulletin du Société Mathematique de France, Springer-Verlag's
	  book series Algorithms and Computation in Mathematics, and the Journal of
	  Software for Algebraic Geometry."
	  },
     PARA{
	  "Eisenbud's interest in computation in the support of commutative algebra and
	  algebraic geometry begin in the early 1970's in the course of his work on free
	  resolutions with David Buchsbaum. An undergraduate named Ray Zibman programmed
	  Brandeis' PDP10 for to compute Gorenstein ideals of codimension 3, and this led
	  to the structure theorem for such ideals. After this success, Eisenbud was a
	  convert to the usefulness of computers in this field, and has been interested
	  in it ever since. His joint work with E. Graham Evans led Evans to suggest the
	  project of computing syzygies to his undergraduate student Mike Stillman..."
	  },
     IMG { "src" => replace("PKG","Style",currentLayout#"package") | "eisenbud.jpg", "alt" => "picture of Eisenbud" }
     }

document {
     Key => "Daniel R. Grayson",
     HREF {"http://www.math.uiuc.edu/~dan/", "Daniel R. Grayson"}, " -- ", HREF {"mailto:dan@math.uiuc.edu", "<dan@math.uiuc.edu>"}, ".",
     PARA{
	  "Daniel Grayson received his PhD in Mathematics from MIT in 1976, taught
	  at Columbia from 1976 to 1981, and became a faculty member at the University of Illinois at
	  Urbana-Champaign in 1981; he retired from the university in 2007.  His mathematical
	  research concerns algebraic K-theory, which is on the theoretical side, but he has always been intrigued
	  by computers.  In 1986 he joined Stephen Wolfram and six other
	  co-authors to write ", HREF { "http://www.wolfram.com/products/mathematica/", ITALIC "Mathematica"}, ",
	  which in the years since its introduction in 1988 has become the pre-eminent 
	  system for mathematics on the computer.",
	  },
     IMG { "src" => replace("PKG","Style",currentLayout#"package") | "grayson2005.jpg", "alt" => "picture of Grayson" }
     }

document {
     Key => "Michael E. Stillman",
     HREF { "http://www.math.cornell.edu/~mike/", "Michael E. Stillman"}, " -- ", HREF {"mailto:mike@math.cornell.edu", "<mike@math.cornell.edu>"}, ".",
     PARA{
	  "Michael E. Stillman received his PhD in Mathematics from Harvard in 1983,
	  taught at University of Chicago 1983-85, was at Brandeis and then MIT 1985-87,
	  and then came to Cornell University.  His mathematical research concerns
	  computational algebraic geometry and algebraic geometry.  He started writing
	  syzygy programs as an undergraduate at the University of Illinois, and from
	  1983 to 1992 with Dave Bayer he wrote Macaulay, a specialized computer
	  algebra system for algebraic geometry and the predecessor of this program.",
	  },
     IMG { "src" => replace("PKG","Style",currentLayout#"package") | "stillman.jpg", "alt" => "picture of Stillman" }
     }

document {
     Key => "Other sources of information about Macaulay2",
     SUBSECTION "Web site",
     UL {
	  (HREF "http://www.math.uiuc.edu/Macaulay2/", " -- the main Macaulay2 web site: citations, binary distributions"),
	  },
     SUBSECTION "Books",
     UL {
	  "Computations in algebraic geometry with Macaulay 2, Algorithms and Computations in Mathematics, no. 8,
	  edited by David Eisenbud, Daniel R. Grayson, Michael E. Stillman, and Bernd Sturmfels,
	  Springer-Verlag, 2001, ISBN 3-540-42230-7.",
	  "Computational Algebraic Geometry, London Mathematical Society Student Texts (No. 58), by Hal Schenck,
	  ISBN-13: 9780521536509, ISBN-10: 0521536502, 2003, 208 pages.",
	  }
     
     }

{* -- Mike wanted this: 
document {
     Key => "Resources required",
     }
*}

document {
     Key => "debugging",
     "Macaulay2 has a debugger.",
	UL{
	TO "the debugger",
	},
	"Here are some other debugging tools.",
     UL {
	  TO "assert",
	  TO "benchmark",
	  TO "Browse::browse",
	  TO "code",
	  TO "current",
	  TO "currentFileName",
	  TO "disassemble",
	  TO "edit",
	  TO "error",
	  TO "errorDepth",
	  TO "flagLookup",
	  TO "listLocalSymbols",
	  TO "listUserSymbols",
     	  TO "locate",
	  TO "methods",
	  TO "on",
	  TO "peek",
	  TO "peek'",
	  TO "profile",
	  TO "shield",
	  TO "showStructure",
	  TO "showUserStructure",
	  TO "try",
	  TO "userSymbols"
	  }
     }

document {
     Key => "system facilities",
     Subnodes => {
     	  "Loading files:",
	  TO "autoload",
	  TO "initialization file",
	  TO "input",
	  TO "load",
	  TO "needs",
     	  "Echoing characters:",
	  TO "clearEcho",
	  TO "setEcho",
     	  "Dumping and restoring the state of the system:",
	  TO "dumpdata",
	  TO "loaddata",
	  TO "restart",
	  TO "addStartFunction",
	  TO "addEndFunction",
     	  "Interface to the operating system:",
	  TO "top level loop",
	  TO "alarm",
	  TO "currentDirectory",
	  TO "exec",
	  TO "exit",
	  TO "fork",
	  TO "getenv",
	  TO "processID",
	  TO "path",
	  TO "quit",
	  TO "run",
	  TO "sleep",
	  TO "time",
	  TO "timing",
	  TO "wait",
     	  "Variables with information about the state of the current process:",
	  TO "commandLine",
	  TO "environment",
	  TO "version",
     	  "Dealing with the garbage collector:",
	  TO "collectGarbage"
	  }
     }

document {
     Key => "initialization file",
     "The file ", TT "init.m2", " is loaded automatically when the
     program is started, if it exists.",
     PARA{
	  "On most systems the file is sought in the directory ", TT "$HOME/.Macaulay2/", ",
	  where ", TT "$HOME", " is replaced by the path to the user's home
	  directory."
	  },
     PARA{
	  "Under Mac OS X, the file is sought instead in the
	  directory ", TT "$HOME/Library/Application Support/Macaulay2/", "."
	  },
     PARA{
	  "If the user wants a file called, say, ", TT "start.m2", " in the current
	  directory to be loaded automatically when the program is started, then the
	  following line of code can be placed in the file ", TT "init.m2", "."
	  },
     PRE {
	  ///if fileExists "start.m2" then load(currentDirectory()|"start.m2")///
	  },
     PARA {
	  "Warning: former versions of the program would also load a file named ", TT "init.m2", " found in the current directory."
	  }
     }

document {
     Key => "combinatorial functions",
     UL {
	  TO "random",
	  TO "binomial",
	  TO "subsets",
	  TO "tally",
	  TO "partitions"
	  }
     }

document {
     Key => "top level loop",
     "The top level evaluation loop of the interpreter contains hooks so the user can
     control how printing of the results of evaluation is done.  If the result is 
     ", TO "null", " then nothing is printed.  Otherwise, the appropriate method
     associated with the symbol ", TO "Print", " is applied to perform the printing,
     unless the printing is to be suppressed, as indicated by a semicolon at the end
     of the statement, in which case the ", TO "NoPrint", " method is applied.",
     UL {
	  TO "AfterEval",
	  TO "AfterPrint",
	  TO "AfterNoPrint",
	  TO "NoPrint",
	  TO "Print",
	  }
     }

document {
	Key => "handling hypertext",
     "Output formatting routines:",
     UL {
	  TOH "html",
	  TOH "mathML",
	  TOH "tex",
	  TOH "info",
	  TOH "net"
	  },
	}



-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
