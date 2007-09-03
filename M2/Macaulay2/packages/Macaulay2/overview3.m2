--		Copyright 1993-1999 by Daniel R. Grayson


document {
     Key => "COPYING",
     Headline => "the Macaulay 2 license agreement",
     "This is the text of the license agreement under which Macaulay 2 is distributed.",
     PARA{},
     if sourceHomeDirectory =!= null then PRE separate("\f",get(sourceHomeDirectory | "COPYING"))
     else if prefixDirectory =!= null then PRE separate("\f",get(prefixDirectory | LAYOUT#"docm2rel" | "COPYING"))
     else (
	  stderr << "warning: can't locate file \"COPYING\"" << endl;
	  SPAN {"See the GNU GENERAL PUBLIC LICENSE, Version 2, June 1991, available at ", HREF "http://www.gnu.org/licenses/gpl.txt", "."}
	  )
     }

document {
     Key => "Copyright and license",
     "Macaulay 2, its object code and source code, and its documentation,
     are copyright by Daniel R. Grayson and Michael E. Stillman.  We permit 
     you to use it under the terms of the GNU General Public License, version
     2, as published by the Free Software Foundation, and as contained in the file ", TT "COPYING", " accompanying the program.",
     Subnodes => {
     	  TOH "COPYING",
      	  "The following free libraries have been compiled into Macaulay 2.",
	  TO "Singular-Factory",
	  TO "Singular-Libfac",
	  TO "GNU MP",
	  TO "GC garbage collector"
	  }
     }

document {
     Key => "GC garbage collector",
     "Macaulay 2 uses the excellent garbage collector GC, version ", version#"gc version", ",
     written by Hans-J. Boehm and Alan J. Demers and generously licensed
     to the public.  It is available at
     ", HREF "http://www.hpl.hp.com/personal/Hans_Boehm/gc/", ".",
     PARA{},
     SeeAlso => {"collectGarbage", "GC error messages" }
     }

document {
     Key => "GC error messages",
     PARA {
     	  "The ", TT "GC garbage collector", " is used by Macaulay 2 for its memory management.  It is reponsible
     	  for getting more memory from the operating system when needed for storage of data, and for recovering areas of memory no longer needed."
	  },
     PARA {
	  "Here is a list of error messages you may see from it when it aborts the program, due to lack of memory or related problems.
	  Typically, the only recourse for the user is to increase the memory available to the program."
	  },
     UL {
	  "Insufficient space for initial table allocation",
	  "No space for lwp data structures",
	  "Out of memory",
	  "Too many exclusions",
	  "Too many heap sections: Increase MAXHINCR or MAX_HEAP_SECTS",
	  "Too many root sets"
	  }
     }

document {
     Key => "Singular-Factory",
     "With the kind permission of the authors of Singular, 
     G.-M. Greuel, R. Stobbe, G. Pfister, H. Schoenemann, and J. Schmidt,
     University of Kaiserslautern, Macaulay 2 incorporates ", TT "Singular-Factory", ",
     version ", version#"factory version", ", a free library of polynomial routines
     that provides for factorization of polynomials.  It is distributed under the
     terms of the GNU General Public License and is available at 
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
     Key => "Singular-Libfac",
     "With the kind permission of the author, Michael Messollen, Macaulay 2
     incorporates ", TT "Singular-Libfac", ", version ", version#"libfac version", ",
     a free library of routines, depending on ", TO "Singular-Factory", ", that provides
     factorization of multivariate polynomials over finite fields and computation of the minimal associated primes 
     of ideals via characteristic sets.  It is distributed under the terms of the
     GNU General Public License, and is available at 
     ", HREF "ftp://www.mathematik.uni-kl.de/pub/Math/Singular/Libfac"," and at 
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
     Key => "GNU MP",
     "The GNU MP library, gmp, version ", version#"gmp version", " provides
     routines for arbitrary precision integer and floating point arithmetic,
     and is distributed under the terms of the GNU Lesser General Public License.
     It's available at ", HREF "ftp://ftp.gnu.org/gnu/gmp/", " and 
     ", HREF "http://www.swox.com/gmp/", "."
     }

document {
     Key => "Acknowledgements",
     "We thank the National Science Foundation for generous funding since
     1993 for this project, Gert-Martin Greuel and Ruediger Stobbe for the
     incorporation of their library ", TO "Singular-Factory", ", Michael Messollen for
     the incorporation of his library ", TO "Singular-Libfac", ",
     and David Eisenbud, Wolfram Decker and Sorin Popescu for
     early support, encouragement and suggestions.  We also acknowledge an
     intellectual debt to Dave Bayer, who, with Michael Stillman,
     wrote Macaulay, a specialized computer algebra system for algebraic
     geometry and the predecessor of this program."
     }

document {
     Key => "preface",
     }

document {
     Key => "LAYOUT",
     Headline => "relative locations of Macaulay 2 files",
     PARA {
	  "The hash table ", TT "LAYOUT", " is a translation table from symbolic names to directory paths,
	  which are to interpreted relative to the path stored in ", TO "prefixDirectory", ".
	  Some of the entries are strings: these provide paths to various types of files associated with
	  the main Macaulay 2 system.  The remaining entries are functions.  These provide paths to various
	  types of files associated with packages, and accept a string containing the name of the package
	  as argument.  Finally, for convenience, many Macaulay 2 files are regarded as being associated
	  with a special package called ", TO "Core", ", and the corresponding documentation files
	  are part of the package ", TT "Macaulay2", "."
     	  },
     EXAMPLE "LAYOUT",
     PARA {
	  "Let's apply the functions above to the name of a fictional package named \"", TT "FOO", "\" to see what paths are returned."
     	  },
     EXAMPLE ///applyValues (LAYOUT, f -> if instance(f,Function) then f "FOO" else f)///,
     "Here are the meanings of the keys used in ", TO "LAYOUT", ".  Some of these entries may not be in use.",
     UL {
	  LI { TT format "bin", " : executable files (M2)" },
	  LI { TT format "cache", " : cached data files (used by dumpdata and loaddata)" },
	  LI { TT format "data", " : machine independent data files, not just for Macaulay 2" },
	  LI { TT format "datam2", " : Macaulay 2 source files (*.m2) for Macaulay 2; this directory appears on the ", TO "path" },
	  LI { TT format "doc", " : documentation, not just for Macaulay 2" },
	  LI { TT format "docm2", " : documentation for Macaulay 2 and its packages" },
	  LI { TT format "docm2rel", " : documentation for Macaulay 2 itself (the Core package)" },
	  LI { TT format "docpackages", " : documentation for Macaulay 2 packages" },
	  LI { TT format "emacs", " : emacs source files (*.el, *.elc)" },
	  LI { TT format "emacsdoc", " : documentation for using Macaulay 2 itself with emacs" },
	  LI { TT format "images", " : images for Macaulay 2 itself" },
	  LI { TT format "info", " : documentation in info form, not just for Macaulay 2" },
	  LI { TT format "lib", " : machine dependent data and executable files, not just for Macaulay 2" },
	  LI { TT format "libm2", " : machine dependent data and executable files for Macaulay 2 itself" },
	  LI { TT format "m2", " : source files for Macaulay 2 itself (the Core package: *.m2)" },
	  LI { TT format "man", " : man pages" },
	  LI { TT format "man1", " : man pages, section 1" },
	  LI { TT format "man3", " : man pages, section 3" },
	  LI { TT format "libraries", " : files associated with third party libraries linked with Macaulay 2" },
	  LI { TT format "packagecache", " : cached data files for the Macaulay 2 package FOO" },
	  LI { TT format "packagedoc", " : documentation for the Macaulay 2 package FOO" },
	  LI { TT format "packageexampleoutput", " : example output files for the Macaulay 2 package FOO" },
	  LI { TT format "packagehtml", " : html documentation for the Macaulay 2 package FOO (*.html)" },
	  LI { TT format "packageimages", " : images for the Macaulay 2 package FOO (*.jpg)" },
	  LI { TT format "packages", " : source files for Macaulay 2 packages; this directory appears on the ", TO "path" },
	  LI { TT format "packagesrc", " : additional source files for the Macaulay 2 package FOO" },
	  LI { TT format "packagetests", " : test files  for the Macaulay 2 package FOO" },
	  LI { TT format "share", " : machine independent files for Macaulay 2 itself" },
	  LI { TT format "style", " : style files for Macaulay 2 itself" }
	  }
     }

document {
     Key => "mathematical examples",
     "In this section we present some tutorials which aim to introduce
     the user to some mathematical ways of using Macaulay 2.  The tutorials
     are relatively independent of each other, and each one introduces the use
     of some features of Macaulay 2 in a slow and leisurely way, assuming the
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
	  TO "Elementary uses of Groebner bases I.  Math 634 Fall 2005",
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


document { Key => "frequently encountered problems",
     }

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
     	  "The authors of Macaulay 2 and the bulk of this manual:",
	  TO "Daniel R. Grayson",
	  TO "Michael E. Stillman",
     	  "Our co-author for the tutorials:",
	  TO "David Eisenbud",
	  }
     }

document {
     Key => "David Eisenbud",
     HREF {"http://www.msri.org/people/staff/de/", "David Eisenbud "}, " ",
     HREF {"mailto:de@msri.org", "<de@msri.org>"}, ".",
     PARA{},
     "In this spot will go a brief biography of David Eisenbud."
     }

document {
     Key => "Daniel R. Grayson",
     HREF {"http://www.math.uiuc.edu/~dan/", "Daniel R. Grayson"}, " ",
     HREF {"mailto:dan@math.uiuc.edu", "<dan@math.uiuc.edu>"}, ".",
     PARA{},
     "Daniel Grayson received his PhD in Mathematics from MIT in 1976, taught
     at Columbia from 1976 to 1981, and came to the University of Illinois at
     Urbana-Champaign in 1981, where he is a Professor.  His mathematical
     research concerns algebraic K-theory, but he has always been intrigued
     by computers.  In 1986 he joined with Stephen Wolfram and six other
     co-authors to write ", ITALIC "Mathematica", " which in the years since
     its introduction in 1988 has become the pre-eminent system for
     mathematics on the computer.",
     PARA{},
     IMG { "src" => "../../Images/Grayson2.jpg", "alt" => "picture of Grayson" }
     }

document {
     Key => "Michael E. Stillman",
     HREF { "http://www.math.cornell.edu/~mike/", "Michael E. Stillman"}, " ",
     HREF {"mailto:mike@math.cornell.edu", "<mike@math.cornell.edu>"}, ".",
     PARA{},
     "Michael E. Stillman received his PhD in Mathematics from Harvard in 1983,
     taught at University of Chicago 1983-85, was at Brandeis and then MIT 1985-87,
     and then came to Cornell University.  His mathematical research concerns
     computational algebraic geometry and algebraic geometry.  He started writing
     syzygy programs as an undergraduate at the University of Illinois, and from
     1983 to 1992 with Dave Bayer he wrote Macaulay, a specialized computer
     algebra system for algebraic geometry and the predecessor of this program."
     }

document {
     Key => "Other sources of information about Macaulay 2",
     SUBSECTION "Web sites",
     UL {
	  (HREF "http://www.math.uiuc.edu/Macaulay2/", " -- the main Macaulay 2 web site: citations, binary distributions"),
	  (HREF "http://sf.net/projects/macaulay2/", " -- the Sourceforge Macaulay 2 web site: source code, bug reporting")
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

document {
     Key => "Resources required",
     }


document {
     Key => "debugging",
     "Macaulay 2 has a debugger.",
	UL{
	TO "the debugger",
	},
	"Here are some other debugging tools.",
     UL {
	  TO "assert",
	  TO "benchmark",
	  TO "Browse::browse",
	  TO "code",
	  TO "currentFileName",
	  TO "edit",
	  TO "error",
	  TO "errorDepth",
	  TO "flagLookup",
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
     PARA{},
     "On most systems the file is sought in the directory ", TT "$HOME/.Macaulay2/", ",
     where ", TT "$HOME", " is replaced by the path to the user's home
     directory.",
     PARA{},
     "Under Mac OS X, the file is sought instead in the
     directory ", TT "$HOME/Library/Application Support/Macaulay2/", ".",
     PARA{},
     "If the user wants a file called, say, ", TT "start.m2", " in the current
     directory to be loaded automatically when the program is started, then the
     following line of code can be placed in the file ", TT "init.m2", ".",
     PRE {
	  ///if fileExists "start.m2" then load(currentDirectory|"start.m2")///
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
