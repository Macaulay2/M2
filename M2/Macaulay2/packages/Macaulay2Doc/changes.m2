-- -*- coding:utf-8 -*-
document {
     Key => "changes to Macaulay 2, by version",
     Subnodes => {
	  TO "changes, 1.0 and 1.1",
	  TO "changes, 1.2"
	  }
     }

document {
     Key => "changes, 1.2",
     UL {
	  LI {
	       "new functions:",
	       UL {
		    TO groupID,
		    TO heft,
		    TO "IntegralClosure :: idealizerReal",
		    TO insert,
		    TO inversePermutation,
		    TO isSorted,
		    TO multidegree,
		    TO runLengthEncode,
		    TO selectVariables,
		    TO "step",
		    TO switch,
		    }
	       },
	  LI {
	       "new methods for old functions:",
	       UL {
		    TO (all,ZZ,Function),
		    TO (any,ZZ,Function),
		    TO (degreesMonoid,List),
		    TO (degreesRing,GeneralOrderedMonoid),
		    TO (degreesRing,List),
		    TO (export,String),
		    TO (findFiles,List),
		    TO (flattenRing,Ideal),
		    TO (gcd,RingElement,ZZ),
		    TO (gcd,ZZ,RingElement),
		    TO (indices,Matrix),
		    TO (map,Module,Module,RingMap,Matrix),
		    TO (map,Module,Nothing,RingMap,Matrix),
		    TO (map,Module,RingMap),
		    TO (map,Module,ZZ,ZZ),
		    TO (max,GradedModule),
		    TO (min,GradedModule),
		    TO (part,InfiniteNumber,InfiniteNumber,VisibleList,RingElement),
		    TO (part,InfiniteNumber,InfiniteNumber,RingElement),
		    TO (part,InfiniteNumber,ZZ,VisibleList,RingElement),
		    TO (part,InfiniteNumber,ZZ,RingElement),
		    TO (part,Nothing,Nothing,VisibleList,RingElement),
		    TO (part,Nothing,Nothing,RingElement),
		    TO (part,Nothing,ZZ,VisibleList,RingElement),
		    TO (part,Nothing,ZZ,RingElement),
		    TO (part,ZZ,InfiniteNumber,VisibleList,RingElement),
		    TO (part,ZZ,InfiniteNumber,RingElement),
		    TO (part,ZZ,VisibleList,RingElement),
		    TO (part,ZZ,Nothing,VisibleList,RingElement),
		    TO (part,ZZ,Nothing,RingElement),
		    TO (part,ZZ,ZZ,VisibleList,RingElement),
		    TO (part,ZZ,ZZ,RingElement),
		    TO (quotientRemainder,Number,RingElement),
		    TO (quotientRemainder,RingElement,Number),
		    TO (quotientRemainder,RingElement,RingElement),
		    TO (scanLines,Function,List),
		    TO (support,Matrix),
		    TO (symbol <-,Sequence),
		    TO (symbol _,Number,Ring),
		    TO (symbol |,GradedModuleMap,GradedModuleMap),
		    TO (symmetricAlgebra,Matrix),
		    TO (symmetricAlgebra,Nothing,Nothing,Matrix),
		    TO (symmetricAlgebra,Nothing,Ring,Matrix),
		    TO (symmetricAlgebra,Ring,Nothing,Matrix),
		    TO (symmetricAlgebra,Ring,Ring,Matrix),
		    TO (symbol ^, RingElement, Ring),
		    TO (symbol ^, Number, Ring),
		    TO (symbol ^, RingElement, RingFamily),
		    TO (symbol ^, Number, RingFamily),
		    TO (symbol ^, Constant, Ring),
		    TO (symbol ^, Constant, RingFamily)
		    }
	       },
	  LI {
	       "new variables:",
	       UL {
		    TO "currentLayout",
		    TO "prefixPath",
		    TO "lastMatch"
		    }
	       },
	  LI {
	       "new symbols:",
	       UL {
		    TO FlatMonoid,
		    TO Join,
		    TO Reduce,
		    TO Result,
		    TO RunExamples,
		    TO SeparateExec,
		    TO SourceRing
		    }
	       },
	  LI {
	       "new optional arguments to functions:",
	       UL {
		    TO [GF,SizeLimit],
		    TO [basis,SourceRing],
		    TO [check,UserMode],
		    TO [fillMatrix, Height],
		    TO [flattenRing,Result],
		    TO [getPackage,Configuration],
		    TO [getPackage,UserMode],
		    TO [hilbertSeries,Reduce],
		    TO [installPackage,CacheExampleOutput],
		    TO [installPackage,RunExamples],
		    TO [installPackage,SeparateExec],
		    TO [installPackage,UserMode],
		    TO [installPackage,Verbose],
		    TO [lift,Verify],
		    TO [map,DegreeLift],
		    TO [monoid,DegreeLift],
		    TO [monoid,Join],
		    TO [newPackage,CacheExampleOutput],
		    TO [newRing,DegreeLift],
		    TO [newRing,DegreeMap],
		    TO [newRing,Join],
		    TO [symmetricAlgebra,DegreeLift],
		    TO [symmetricAlgebra,DegreeMap],
		    TO [symmetricAlgebra,Join],
		    TO [tensor,DegreeLift],
		    TO [tensor,DegreeMap],
		    TO [tensor,Join],
		    }
	       },
	  LI {
	       "new packages:",
	       UL {
		    TO "ConwayPolynomials :: ConwayPolynomials",
		    TO "EdgeIdeals :: EdgeIdeals",
		    TO "FourTiTwo :: FourTiTwo",
		    TO "Polyhedra :: Polyhedra",
		    TO "SimpleDoc :: SimpleDoc",
		    TO "StatePolytope :: StatePolytope",
		    TO "Text :: Text"
		    }
	       },
	  LI {
	       "miscellaneous changes:",
	       UL {
		    LI {"The behavior of ", TO "return", " in the debugger has changed: it now stops in the debugger
			 at the next available opportunity."
			 },
		    LI {"Filenames starting with ", TT "~/", " will have the tilde replaced by the home directory."},
		    LI {"The ", EM "D", " language, in which the Macaulay 2 interpreter is written, is now type-safe"},
		    LI {"During compilation of Macaulay 2, as much of the computation as possible is now done
			 to satisfy the make-target ", TT "all", ", with the resulting files placed in a staging
			 area, ready for quickly satisfying the make-target ", TT "install", "."
			 },
		    LI {"During compilation of Macaulay 2, it is now possible to keep the architecture independent
			 files in a separate directory tree, saving time if versions for multiple architectures
			 are to be built.  Including those files in a source tar file will allow the distribution
			 of a ", EM "fat", " source tar file, speeding up compilation."
			 },
		    LI {"Pressing the RET key in the Macaulay 2 interaction buffer on a line containing
			 a source file name and line number will open up the source file at that position in
			 a new buffer.  This allows error messages to be treated with dispatch."
			 },
		    LI {"The behavior of ", TO "setup", " has changed, in that the commands that set the paths are
			 now placed in separate files in the home directory of the user, and those files are
			 sourced only if they are present.  That enables the user to continue to share the usual 
			 init files on multiple machines, even though Macaulay 2 may be installed in various different
			 locations."
			 },
		    LI {"The function ", TO "hilbertFunction", " is now faster at computing power series expansions."},
		    LI {"A new division algorithm has been implemented in rings with inverses,
			 where the monomials can involve negative exponents, and hence are not well-ordered.
			 See ", TO "division in polynomial rings with inverses", "."
			 },
		    LI {"A bug in ", TO "irreducibleCharacteristicSeries", ", upon with ", TO "minimalPrimes", "
			 depends, was fixed.  Now the new ring supporting the characteristic series will
			 have variables with the same names and degrees, but the ordering of the variables
			 and the monomial ordering will be different.  This ensures that homogeneity will
			 be preserved.  Also, for convenience, the routine
			 now returns a sequence, instead of a list, suitable for immediate parallel assignment."
			 },
		    LI {"The function ", TO "minors", " has been altered so the ideal of ", TT "n", " by ", TT "n", " minors that it provides for negative
			 values of ", TT "n", " is the unit ideal."
			 },
		    LI {"The function ", TO "part", " has been altered so that for multigraded rings, it does not use the first component
			 of the degree vector.  New functionality has been added, and the method for ", TT "part(Sequence,RingElement)", "
			 has been removed."
			 },
		    LI {"Heft vectors are now automatically computed, ", TO "heft vectors"},
		    LI {"The description of a ring provided by ", TO "describe", " is now abbreviated by making use of
			 run length encoding"
			 },
		    LI {"Browsers started by ", TO "viewHelp", " are now run in a separate process group
			 so they don't die when Macaulay 2 terminates."
			 },
		    LI {"The function ", TO "prune", " and ", TO "decompose", " are no longer synonyms of ", TO "minimalPresentation", " and ", TO "minimalPrimes", ",
			 respectively"
			 },
		    LI {"The function ", TO "get", " has been fixed so it returns an error message if the process associated with the pipe has died."},
		    LI {"The function ", TO "searchPath", " now does what the documentation said it would do."},
		    LI {"Comparison of rings with ", TO "==", " instead of ", TO "===", " is no longer supported."},
		    LI {"The output operation ", TT "s<<x", ", when ", TT "s", " is a string, has been changed
			 so that if a file with filename ", TT "s", " is already open, that file will be used
			 instead of opening a new file with the same name, erasing the data already written
			 to the file."
			 },
		    LI {"Tensor product of a module with a ring has been modified so it will make a ring map
			 between the two rings that is derived from the names of the variables."
			 },
		    LI {
			 TO "GF", " now uses Conway polynomials when possible.  Maps between Galois fields
			 made with them are now easy to produce with ", TT "map(E,F)", "."
			 },
		    LI {
			 "The degrees in symmetric algebras have been corrected."
			 },
		    LI {
			 "The default for polynomial rings over polynomial rings is now to join
			 the degree vectors of monomials in the base to the degree vectors of
			 the top level monoid, usually resulting in a multigraded ring.  For example,
			 QQ[x][y] is now bigraded."
			 },
		    LI {
			 "Inverting a noninvertible matrix results in an error message now."
			 },
		    LI {
			 "The function ", TO "export", ", given a string (rather than a symbol) will now make a new
			 symbol with that name, even if a symbol with the same name already exists 
			 in another visible package."
			 },
		    LI {
			 "Homomorphisms (maps) of modules over different rings with respect to a ring homomorphism
			 between them are now supported.  Composition, coimage (replacing pushForward1), and kernel work.
			 An option has been added to ", TO "basis", " to ask it to return such a homomorphism.  See ",
			 TO (map,Module,Module,RingMap,Matrix), ", ", TO (map,Module,Nothing,RingMap,Matrix), ", and ",
		    	 TO (map,Module,RingMap), "."
			 },
		    LI {
			 "The function ", TO "basis", " will now check finite dimensionality in advance, to avoid running out of memory."
			 },
		    LI {
			 "Functions and types associated with hypertext and documentation have been isolated in a new package
			 called ", TO2{"Text::Text","Text"}, ", which gets loaded automatically by ", TO "beginDocumentation", "."
			 },
		    LI {
			 TT "errorCode", " has been renamed to ", TO "current"
			 },
		    LI {
			 "The file layout system, as described by ", TO "Layout", ", formerly called ", TT "LAYOUT", ", now 
			 supports separation of architecture independent files from architecture dependent files."
			 },
		    LI {
			 "The cross reference hyperlinks in the info form of the documentation have been improved,
			 but we recommend reading it in emacs with ", TT "M-x info", ", configuring the emacs
			 variable ", TT "Info-hide-note-references", " so its value is ", TT "hide", ".
			 See ", TO "reading the documentation", "."
			 },
		    LI {
			 "Now the function ", TO "needs", " will reload the requested file not only if the file has
			 not been loaded before, but also if it has changed since the previous time."
			 },
		    LI {
			 "It is now possible to represent a series of three or more slashes within a string delimited by
			 ", TO "///", " by typing a longer series."
			 },
		    LI {
			 "The conversion of ", TO "TEX", " to html has been improved and documented, see ", TO "Text::html(TEX)", "."
			 },
		    LI {
			 "Unicode, encoded in ", TT "utf-8", " format, is supported in documentation pages, both in html form and in 
			 emacs info form: 你好."
			 },
		    LI {
			 "When an error occurs within a string being evaluated with ", TO "value", ", the 
			 appropriates lines of the string will be displayed if the debugger is entered."
			 },
		    LI {
			 "A new method for ", TO "substring", " accepts a pair of integers as first argument
			 of the sort returned by ", TO "regex", "."
			 },
		    LI {
			 "Regular expression handling, by the functions ", TO "regex", ", ", TO "match", ", ", TO "replace", ", and ", TO "select", ",
			 is now much faster because strings are not copied."
			 },
		    LI {
			 "The function ", TO "regex", " now has a form which restricts the range of the search."
			 },
		    LI {
			 "The total Ext functor now accepts multigraded modules, see ", TO "(Ext,Module,Module)", "."
			 },
		    LI {
			 "Macaulay 2 now incorporates ", TO "frobby", ", a free library for computing
			 the Alexander dual of a monomial ideal (see ", TO "(dual,MonomialIdeal)", ")."
			 },
		    LI {
			 "The function ", TO "select", " will now give an error message if the
			 function provided to it returns something neither ", TO "true", " nor ", TO "false", "."
			 },
		    LI {
			 "Macaulay 2 now incorporates ", TO "pari", ", a free library for computing in number theory.
			 It is used by ", TO (factor,ZZ), ", ", TO (factor,QQ), ", ", TO (isPseudoprime, ZZ), ", and ", TO (isPrime,ZZ), "."
			 },
		    LI {
			 "The quotient and remainder for two ring elements can now be obtained simultaneously,
			 saving time.  See ", TO (quotientRemainder,RingElement,RingElement), "."
			 },
		    LI {
			 "The binary representation of a real number is now available using ", TO (promote,RR,QQ), ".
			 The code for ", TO (lift,RR,QQ), " has been tightened up so a rational number is provided
			 that provides exactly the same real number when promoted."
			 },
		    LI {
			 "The emacs commands ", TT "M-x M2", ", bound to ", TT "f12", ", and ", TT "M2-send-to-program", ", 
			 bound to ", TT "f11", ", have some new capability.  
     	       	    	 With prefix argument ", TT "C-u C-u", " to ", TT "M2", ", the tag from which the buffer name is constructed (by
			 prepending and appending asterisks) can be entered in the minibuffer.
		         With a prefix argument to ", TT "M2-send-to-program", ", the name of
			 the buffer to which this and future uses of the command (in this buffer) should
			 be sent can be entered, with history."
			 },
		    LI {
			 "The function ", TO "symmetricAlgebra", " is now functorial."
			 },
		    LI {
			 "The variable ", TT "randomHeight", " has been removed, replaced by 
			 ", TO [fillMatrix, Height], " and ", TO [random,Height], "."
			 }
		    }
	       }
	  }
     }

document {
     Key => "changes, 1.0 and 1.1",
     PARA ///
     Versions have been compiled specifically for the following GNU/Linux
     systems: generic Linux, Ubuntu (32 bit and 64 bit), Debian (32 bit and 64
     bit) both with *.deb files, Fedora 7, Fedora 8, and Red Hat Enterprise 4,
     with *.rpm files; for the following Macintosh OS X systems: 10.4 and 10.5
     on Intel 32 bit, 10.5 on Intel 64 bit, and 10.4 on the Power PC; and on
     Microsoft Windows with the Cygwin compatibility package installed.
     Automatic installation from our repositories is possible for Debian,
     Ubuntu, and Microsoft Windows.  The files for downloading are now divided
     into two archives, depending on whether they depend on the architecture.
     ///,
     PARA {
	  "Packages have been contributed: ", 
	  TO2{ "NoetherNormalization::NoetherNormalization","NoetherNormalization"},
	  ", by Bart Snapp and Nathaniel Stapleton;
	  ", TO2{"GenericInitialIdeal::GenericInitialIdeal","GenericInitialIdeal"}, " and
	  ", TO2{"Regularity::Regularity","Regularity"}, ",
	  by Alexandra Seceleanu and Nathaniel Stapleton;
	  ", TO2{"InvolutiveBases::InvolutiveBases","InvolutiveBases"}, ", by Daniel Robertz;
	  ", TO2{"ChainComplexExtras::ChainComplexExtras","ChainComplexExtras"}, ", by Frank Moore and Greg Smith;
	  ", TO2{"HyperplaneArrangements::HyperplaneArrangements","HyperplaneArrangements"}, ", by Graham Denham and Gregory G. Smith;
	  ", TO2{"LexIdeals::LexIdeals","LexIdeals"}, ", by Chris Francisco;
	  ", TO2{"ReesAlgebra::ReesAlgebra","ReesAlgebra"}, ", by David Eisenbud, Amelia Taylor, and Sorin Popescu; and
	  ", TO2{"TangentCone::TangentCone","TangentCone"}, ", by Craig Huneke and David Eisenbud."
	       },
     PARA {"A good implementation of real and complex numbers to arbitrary precision,
	  based on the mpfr library from ", HREF "http://mpfr.org/", ", has been implemented.  The
	  library is remarkable for the care taken to return correctly rounded
	  results.  It is hoped that this will form a good base for experimentation
	  with algebraic algorithms that mix symbolic and numeric techniques.  Basic
	  transcendental functions are also provided, and pi is now a symbolic
	  constant usable in numeric expressions of any precision.  An interface to
	  lapack routines for singular value decomposition and eigenvectors is
	  provided (but they operate only with 53 bits of precision).
	  "},
     PARA ///
     An interface with TeXmacs has been provided, so Macaulay 2 can be run with
     a good graphical user interface.  More work remains to be done, but it is
     usable.
     ///,
     PARA ///
     Documentation has been improved, with every function documented.
     ///,
     PARA ///
     Computation of Gröbner bases over local rings has been improved.  New
     notation QQ{x,y,z} for local rings.  More precisely
     ///,
     PARA ///
     The default (GRevLex) monomial ordering in polynomial rings whose
     variables don't all have degree 1 was fixed to take the degrees into
     account.  More precisely, the ordering now uses the values obtained by
     scalar product of the provided heft vector with the degree vector.
     ///,
     PARA ///
     The implementation of the Gröbner basis algorithm for polynomial rings
     where the multi-degrees of the variables don't all have strictly positive
     first component has been fixed by having it use the heft vector provided.
     The problem was that bases were not minimalized, and S-pairs were
     addressed in a non-optimal order.  (The total Ext functor Ext(M,N) used
     this facility and was returning wrong answers.)
     ///,
     PARA ///
     A bug in division (f//g) resulting in incorrect answers over quotient
     rings was fixed.
     ///,
     PARA {"A bug in ", TO "trim", " and ", TO "mingens", " resulting in incorrect answers was fixed."},
     PARA ///
     A bug in computation of the Gröbner basis of an exterior algebra over Z
     was fixed.
     ///,
     PARA {
	  "A bug in fraction division was fixed.  Fraction field code now checks for
     	  non-units in many more places.  For rings that have been declared by the
     	  user to be fields, and yet are not fields, attempting to divide by a
     	  non-unit results in an error, and sets a value so that the function
     	  ", TO "getNonUnit", " returns that value."
	  },
     PARA ///
     The Gröbner basis routine can now handle large monomial ideals without a
     stack overflow.
     ///,
     PARA {"The function ", TO "monomialIdeal", ", over polynomial rings over ", TO "ZZ", ", now incorporates
     	  leading monomials with nonzero coefficients.  Formerly the coefficients
     	  had to be units."
	  },
     PARA ///
     Codimension (and dimension) computations over polynomial rings over Z work
     once again.
     ///,
     PARA ///
     The speed of computation of projective resolutions when the first
     components of the degrees of the variables are not necesarily positive has
     been improved.
     ///,
     PARA ///
     The interpreter has been fixed so it more often detects extreme recursion;
     one case was omitted that allowed the machine stack to overflow with a
     segmenation fault.
     ///,
     PARA ///
     The function "betti" now returns a new type of object of class BettiTally,
     which can be manipulated with the operations that can manipulate chain
     complexes.
     ///,
     PARA {"Support for utf-8 encoding of unicode characters in strings provided via ", TO "utf8", "."},
     PARA {"A new function ", TO "scanLines", " can be used for reading a big file one line at a time."},
     PARA ///A new format for multi-line block comments is {* ... *}.///,
     PARA ///M2 can now be run with script files by using///,
     PRE ///       #! /usr/bin/M2 --script///, 
     PARA ///as the first line of the script file.///,
     PARA ///
     Under Microsoft Windows, the links in the html form of the documentation
     now work in such a way that browsers can follow them, and viewHelp now
     works (if it finds firefox).
     ///,
     PARA {
	  "Here are the functions added to the Core package since 0.9.95: ",
	  TO "acosh", ", ",
	  TO "acot", ", ",
	  TO "agm", ", ",
	  TO "ancestors", ", ",
	  TO "asinh", ", ",
	  TO "atan2", ", ",
	  TO "BesselJ", ", ",
	  TO "BesselY", ", ",
	  TO "clean", ", ",
	  TO "commonest", ", ",
	  TO "commonRing", ", ",
	  TO "cot", ", ",
	  TO "coth", ", ",
	  TO "cpuTime", ", ",
	  TO "csc", ", ",
	  TO "csch", ", ",
	  TO "debugError", ", ",
	  TO "default", ", ",
	  TO "eint", ", ",
	  TO "erf", ", ",
	  TO "erfc", ", ",
	  TO "expm1", ", ",
	  TO "fillMatrix", ", ",
	  TO "Gamma", ", ",
	  TO "gbRemove", ", ",
	  TO "gbSnapshot", ", ",
	  TO "getSymbol", ", ",
	  TO "globalAssign", ", ",
	  TO "httpHeaders", ", ",
	  TO "installHilbertFunction", ", ",
	  TO "instances", ", ",
	  TO "isANumber", ", ",
	  TO "isFinite", ", ",
	  TO "isInfinite", ", ",
	  TO "isReal", ", ",
	  TO "lngamma", ", ",
	  TO "log1p", ", ",
	  TO "LUdecomposition", ", ",
	  TO "markedGB", ", ",
	  TO "norm", ", ",
	  TO "openOutAppend", ", ",
	  TO "parts", ", ",
	  TO "powermod", ", ",
	  TO "scanLines", ", ",
	  TO "sec", ", ",
	  TO "sech", ", ",
	  TO "seeParsing", ", ",
	  TO "setupEmacs", ", ",
	  TO "size2", ", ",
	  TO "toCC", ", ",
	  TO "toRR", ", ",
	  TO "utf8", ", ",
	  TO "Wikipedia", ", and ",
	  TO "zeta", "."
	  },
     PARA ///
     Compilation of Macaulay 2 from source has been improved.  Needed third
     party libraries will now be downloaded and compiled automatically if they
     are not already provided.
     ///,
     PARA ///
     More tests have been added (to verify, after compilation, that M2 is
     working as expected).
     ///
     }
