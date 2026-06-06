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
     Key => {"Copyright and license", "copyright"},
     PARA {
	  "Macaulay2, its object code, source code, and documentation,
	  are copyright by ",
	  HREF{"https://github.com/Macaulay2/M2/wiki/The-Macaulay2-Authors",
	      "The Macaulay2 Authors"}, ".  We permit you to use it either
	  under the terms of the GNU General Public License, version 2, as published by the Free Software Foundation, and as
	  contained in the file ", TO "COPYING-GPL-2", " accompanying the program, or
	  under the terms of the GNU General Public License, version 3, as published by the Free Software Foundation, and as
	  contained in the file ", TO "COPYING-GPL-3", " accompanying the program."
	  },
     PARA {
	  "Various packages are included with Macaulay2 and come with their own copyright notices and licenses, see
	  ", TO "packages provided with Macaulay2", "."
	  },
     PARA {
      	  "Some free libraries have been compiled into (or linked with) Macaulay2, and some free programs, or packages
	  of programs, with their libraries, have been compiled and distributed with Macaulay2."
	  },
     PARA {
	  "Run the command ", M2CODE "copyright", " to view this message."
	  },
     Subnodes => {
	"licenses",
	TO "COPYING-GPL-2",
	TO "COPYING-GPL-3",
	  "libraries",
	  TO "Singular-Factory",
	  TO "frobby",
	  TO "GNU MP",
	  TO "MPFR",
	  TO "MPFI",
	  TO "GC garbage collector",
	  TO "LAPACK",
	  TO "BLAS",
	  TO "NTL library",
	  TO "FLINT",
	  TO "givaro",
	  TO "fflas-ffpack",
	  TO "MPSolve",
	  TO "Boost",
	  "programs and their libraries",
	  TO "4ti2",
	  TO "gfan",
	  TO "normaliz",
	  TO "csdp",
	  TO "nauty",
	  TO "cdd+",
	  TO "lrslib",
	  TO "msolve",
	  TO "topcom",
	  TO "cohomCalg"
     	  }
     }

document {
     Key => "cohomCalg",
     PARA {
     	  "The program ", TT "cohomCalg", ", by Ralph Blumenhagen, Benjamin Jurke, Thorsten Rahn, and Helmut Roschy,
	  computes cohomology of line bundles, and is available from ", HREF "http://wwwth.mppmu.mpg.de/members/blumenha/cohomcalg/", "."
	  }
     }

document {
     Key => "topcom",
     PARA {
	  "The program ", TT "topcom", ", by Jörg Rambau, computes triangulations of point configurations and oriented matroids,
	  and is available from ", HREF "http://www.rambau.wm.uni-bayreuth.de/TOPCOM/", "."
	  }
     }

document {
     Key => "csdp",
     PARA {
	  "The program ", TT "csdp", ", by Brian Borchers, solves semidefinite programming problems
	  and is available from ", HREF "http://www.coin-or.org/download/source/Csdp", "."
	  }
     }

document {
     Key => "givaro",
     PARA {
	  "The library ", TT "givaro", " is a library for arithmetic and algebraic computations, is required for ", TO "fflas-ffpack", ", 
	  and is available from ", HREF "https://github.com/linbox-team/givaro", "."
	  }
     }

document {
     Key => "fflas-ffpack",
     PARA {
	  "The library ", TT "fflas-ffpack", " is a library
	  for dense and sparse linear algebra over a finite field or the ring of integers,
 	  available from ", HREF "https://github.com/linbox-team/fflas-ffpack", "."
	  }
     }

document {
     Key => "FLINT",
     PARA {
	  "The library ", TT "FLINT", " (Fast Library for Number Theory) is a
	  library for computations in number theory, consisting mainly of routines for
	  integer and polynomial arithmetic and linear algebra.  It was written
	  by William Hart, Mike Hansen, Sebastian Pancratz, Fredrik Johansson,
	  and others, and is available at ", HREF "http://flintlib.org/", "."
	  }
     }

document {
     Key => "lrslib",
     PARA {
     	  "The program ", TT "lrslib", ", written by David Avis,
	  provides the reverse search algorithm for  vertex enumeration and convex hull problems.
	  It is available at ", HREF "http://www-cgrl.cs.mcgill.ca/~avis/C/lrs.html", "."
	  }
     }

document {
     Key => "cdd+",
     PARA {
     	  "The program ", TT "cdd+", TEX ", written by Komei Fukuda,
	  is a C++ implementation of the double description 
	  method of Motzkin, et al., for generating all vertices
	  and extreme rays of a general convex polyhedron in $\\RR^d$ given by a system 
	  of linear inequalities.  It is available at ", HREF "http://www.ifor.math.ethz.ch/~fukuda/cdd_home/", "."
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
     	  ", TO "Normaliz::Normaliz", " is an interface to it."
	  }
     }

document {
     Key => "gfan",
     "The program ", TT "gfan", ", written by Anders Jensen, computes Gröbner fans.
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
     Key => "MPFI",
     PARA {
	  "The ", TT "MPFI", " library is used by Macaulay2 for its arbitrary precision real interval arithmetic
	  and associated transcendental functions.  It is available at ", HREF "https://gforge.inria.fr/projects/mpfi/", ".  The
	  library is based on the ", TO "MPFR", " library.  The library is remarkable for the care taken to return correctly rounded
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
	  ", HREF "https://www.hboehm.info/gc/", "."
	  },
     PARA{
	  "Some environment variables can be set by the user to tune garbage collector performance:"
	  },
     UL {
	  LI { "GC_INITIAL_HEAP_SIZE -- initial heap size in bytes, or number of gigabytes followed by 'G', similarly for 'M', 'K'" },
	  LI { "GC_MAXIMUM_HEAP_SIZE -- optional maximum collected heap size" },
	  LI { "GC_FREE_SPACE_DIVISOR -- if set to a number D, then
                         we try to make sure that we allocate at least N/D bytes between collections, where N is twice the
                         number of traced bytes, plus the number of untraced bytes, plus a rough estimate of the root set
                         size.  Increasing its value will use less space but more collection time.  Decreasing it will
                         appreciably decrease collection time at the expense of space." },
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
	  "Here are some error messages you may see from it when it aborts the program, due to lack of memory or related problems."
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
     PARA {
      "You may simply be out of memory, and then the only recourse is to increase the memory available to the program.
      However, if you don't have enough heap sections,
      sometimes one can just start Macaulay2 by setting the GC_INITIAL_HEAP_SIZE environment variable 
      to some larger value with an environment setting prefix on the M2 command line, e.g., ", TT "GC_INITIAL_HEAP_SIZE=20G M2", ".",
	  },
     SeeAlso => { collectGarbage, GCstats }
     }

doc /// 
    Key 
        "MPSolve"
    Headline 
        a library for finding roots of univariate polynomials 
    Description 
        Text 
            Starting with version 1.16, Macaulay2 incorporates the
            MPsolve (version 3) package, available at @HREF
            "https://numpi.dm.unipi.it/_media/software/mpsolve"@,
            and originally developed by Dario Bini, Giuseppe
            Fiorentino, and Leonardo Robol.
            
            This library is used by the @TO "roots"@ function, for
            finding complex roots of a univariate polynomial
    SeeAlso
        (roots, RingElement)
///

doc ///
  Key
    "Boost"
  Headline
    a collection of C++ source libraries
  Description
    Text
      @HREF {"https://www.boost.org/", "Boost"}@ is a collection of free peer-reviewed and
      portable C++ libraries suitable for eventual standardization.

      Macaulay2 incorporates the following Boost libraries:
    Tree
      @HREF {"https://www.boost.org/doc/libs/release/libs/stacktrace/", "Boost.Stacktrace"}@ (since version 1.16)
        :used internally for printing stack traces
      @HREF {"https://www.boost.org/doc/libs/release/libs/regex/", "Boost.Regex"}@ (since version 1.17)
        :used by the @TO regex@ function, as well as other functions using @TO "regular expressions"@.

    Text
      Note: the program @TO "normaliz"@ utilizes the
      @HREF {"https://www.boost.org/doc/libs/release/libs/dynamic_bitset/", "Boost dynamic_bitset class"}@.
///

document {
     Key => "frobby",
     PARA {
	  "Starting with version 1.2, Macaulay2 incorporates ", TT "frobby", ", a free library of routines for computing
	  the Alexander dual of a monomial ideal (see ", TO (dual,MonomialIdeal), ", a method used internally
	       by many routines), written
	  by Bjarke Hammersholt Roune, and available at ", HREF "http://www.broune.com/frobby/", "."
	  }
     }

document {
     Key => "Singular-Factory",
     "With the kind permission of the authors of Singular, 
     G.-M. Greuel, R. Stobbe, G. Pfister, H. Schoenemann, and J. Schmidt,
     University of Kaiserslautern, Macaulay2 incorporates ", TT "Singular-Factory", ",
     version ", version#"factory version", ", a free library of polynomial routines
     that provides for factorization of polynomials.  It is part of ",
     TT "Singular", ", whose home page is ", HREF "https://www.singular.uni-kl.de/", ".",
     PARA{},
     "The following important routines call upon it:",
     UL {
	  TOH "factor",
	  TOH "gcd",
	  TOH "MinimalPrimes :: minimalPrimes",
	  TOH "irreducibleCharacteristicSeries"
	  }
     }

document {
     Key => "4ti2",
     "The package of programs ", TT "4ti2", " is dedicated to algebraic, geometric and combinatorial
     problems on linear spaces.  It is available at ", HREF "http://www.4ti2.de/", ".  The package ", TO "FourTiTwo::FourTiTwo", " runs it."
     }

document {
     Key => "GNU MP",
     "The GNU MP library (GMP) provides
     routines for arbitrary precision integer and floating point arithmetic.
     It is available at ", HREF "https://gmplib.org/", " and ", HREF "https://gmplib.org/", ".
     To see the version of the library your copy of Macaulay2 is linked with, examine the variable ", TO "version", "."
     }

doc ///
  Key
    "msolve"
  Headline
    a library for solving multivariate polynomial systems
  Description
    Text
      The program @SAMP "msolve"@, written by Jérémy Berthomieu, Christian Eder,
      Vincent Neiger, and Mohab Safey El Din, uses advanced Gröbner bases
      algorithms, multi-threading, and vectorization to obtain exact solutions
      for multivariate polynomial systems.  It is distributed under the GPLv2
      license and is available at @HREF "https://msolve.lip6.fr/"@.  It is used
      by the package @TO "Msolve::Msolve"@.
///

document {
     Key => "Acknowledgements",
     "We thank the National Science Foundation for generous funding since
     1993 for this project, Gert-Martin Greuel and Ruediger Stobbe for the
     incorporation of their library ", TO "Singular-Factory", ",
     and David Eisenbud, Wolfram Decker and Sorin Popescu for
     early support, encouragement and suggestions.  We also acknowledge an
     intellectual debt to Dave Bayer, who, with Michael Stillman,
     wrote Macaulay, a specialized computer algebra system for algebraic
     geometry and the predecessor of this program.",
     SeeAlso => "The authors of Macaulay2 packages"
     }

document {
     Key => "The authors of Macaulay2",
     PARA{},
     "To communicate with the authors about the program, use this email address:
     ", HREF {"mailto:macaulay2@googlegroups.com", "<macaulay2@googlegroups.com>"}, ".",
     Subnodes => {
	 -- "The authors of Macaulay2:",
	  TO "Daniel R. Grayson",
	  TO "Michael E. Stillman",
     	  "Our co-author for the tutorials, long time supporter and user of Macaulay2, and collaborator on the project since 2007:",
	  TO "David Eisenbud"},
     SeeAlso => "The authors of Macaulay2 packages"
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
	  boards of the Bulletin du Société Mathématique de France, Springer-Verlag's
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

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
