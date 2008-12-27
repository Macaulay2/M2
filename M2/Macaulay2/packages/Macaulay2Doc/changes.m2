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
		    TO RLE,
		    TO heft,
		    TO "IntegralClosure :: idealizerReal",
		    TO insert,
		    TO inversePermutation,
		    TO isSorted,
		    TO multidegree,
		    TO "step",
		    TO switch,
		    }
	       },
	  LI {
	       "new methods for old functions:",
	       UL {
		    TO (all,ZZ,Function),
		    TO (any,ZZ,Function),
		    TO (degreesRing,List),
		    TO (findFiles,List),
		    TO (flattenRing,Ideal),
		    TO (image,RingMap),
		    TO (map,Module,Module,RingMap,Matrix),
		    TO (map,Module,Nothing,RingMap,Matrix),
		    TO (map,Module,RingMap),
		    TO (map,Ring,Ring,RingMap),
		    TO (max,GradedModule),
		    TO (min,GradedModule),
		    TO (part,ZZ,ZZ,List,RingElement),
		    TO (symmetricAlgebra,Matrix),
		    TO (symmetricAlgebra,Ring,Ring,Matrix),
		    TO (symbol <-, Sequence),
		    }
	       },
	  LI {
	       "new variables:",
	       UL {
		    TO "currentLayout",
		    TO "lastMatch"
		    }
	       },
	  LI {
	       "new optional arguments to functions:",
	       UL {
		    TO [basis, SourceRing],
		    TO [GF, LengthLimit],
		    TO [hilbertSeries, Reduce],
		    TO [installPackage, SeparateExec],
		    TO [lift, Verify],
		    TO [map, DegreeLift],
		    TO [matrix, DegreeLift],
		    TO [monoid, DegreeLift],
		    TO [monoid, Join],
		    TO [symmetricAlgebra, DegreeLift],
		    TO [symmetricAlgebra, DegreeMap],
		    TO [tensor, DegreeMap],
		    TO [tensor, Join],
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
     PARA ///
     Packages have been contributed: NoetherNormalization, by Bart Snapp and
     Nathaniel Stapleton; GenericInitialIdeal and Regularity, by Alexandra
     Seceleanu and Nathaniel Stapleton; InvolutiveBases, by Daniel Robertz;
     ChainComplexExtras, by Frank Moore and Greg Smith; HyperplaneArrangements,
     by Graham Denham and Gregory G. Smith; LexIdeals, by Chris Francisco;
     ReesAlgebra, by David Eisenbud, Amelia Taylor, and Sorin Popescu; and
     TangentCone, by Craig Huneke and David Eisenbud.
     ///,
     PARA ///
     A good implementation of real and complex numbers to arbitrary precision,
     based on the mpfr library from mpfr.org, has been implemented.  The
     library is remarkable for the care taken to return correctly rounded
     results.  It is hoped that this will form a good base for experimentation
     with algebraic algorithms that mix symbolic and numeric techniques.  Basic
     transcendental functions are also provided, and pi is now a symbolic
     constant usable in numeric expressions of any precision.  An interface to
     lapack routines for singular value decomposition and eigenvectors is
     provided (but they operate only with 53 bits of precision).
     ///,
     PARA ///
     An interface with TeXmacs has been provided, so Macaulay 2 can be run with
     a good graphical user interface.  More work remains to be done, but it is
     usable.
     ///,
     PARA ///
     Documentation has been improved, with every function documented.
     ///,
     PARA ///
     Computation of Groebner bases over local rings has been improved.  New
     notation QQ{x,y,z} for local rings.  More precisely
     ///,
     PARA ///
     The default (GRevLex) monomial ordering in polynomial rings whose
     variables don't all have degree 1 was fixed to take the degrees into
     account.  More precisely, the ordering now uses the values obtained by
     scalar product of the provided heft vector with the degree vector.
     ///,
     PARA ///
     The implementation of the Groebner basis algorithm for polynomial rings
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
     PARA ///
     A bug in "trim" and "mingens" resulting in incorrect answers was fixed.
     ///,
     PARA ///
     A bug in computation of the Groebner basis of an exterior algebra over Z
     was fixed.
     ///,
     PARA ///
     A bug in fraction division was fixed.  Fraction field code now checks for
     non-units in many more places.  For rings that have been declared by the
     user to be fields, and yet are not fields, attempting to divide by a
     non-unit results in an error, and sets a value so that the function
     "getNonUnit" returns that value.
     ///,
     PARA ///
     The Groebner basis routine can now handle large monomial ideals without a
     stack overflow.
     ///,
     PARA ///
     The function monomialIdeal, over polynomial rings over Z, now incorporates
     leading monomials with nonzero coefficients.  Formerly the coefficients
     had to be units.
     ///,
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
     PARA ///
     Support for utf-8 encoding of unicode characters in strings provided via
     "utf8".
     ///,
     PARA ///
     A new function "scanLines" can be used for reading a big file one line at
     a time.
     ///,
     PARA ///
     A new format for multi-line block comments is {* ... *}.
     ///,
     PARA ///
     M2 can now be run with script files by using///, PRE ///       #! /usr/bin/M2 --script///, 
     PARA ///
     as the first line of the script file.
     ///,
     PARA ///
     Under Microsoft Windows, the links in the html form of the documentation
     now work in such a way that browsers can follow them, and viewHelp now
     works (if it finds firefox).
     ///,
     PARA ///
     Here are the functions added to the Core package since 0.9.95: acosh,
     acot, agm, ancestors, asinh, atan2, BesselJ, BesselY, clean, commonest,
     commonRing, cot, coth, cpuTime, csc, csch, debugError, default, eint, erf,
     erfc, expm1, fillMatrix, Gamma, gbRemove, gbSnapshot, getSymbol,
     globalAssign, httpHeaders, installHilbertFunction, instances, isANumber,
     isFinite, isInfinite, isReal, lngamma, log1p, LUdecomposition, markedGB,
     norm, openOutAppend, parts, powermod, scanLines, sec, sech, seeParsing,
     setupEmacs, size2, toCC, toRR, utf8, Wikipedia, zeta.
     ///,
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
