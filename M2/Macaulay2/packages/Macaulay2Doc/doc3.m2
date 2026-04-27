--		Copyright 1993-1998 by Daniel R. Grayson

document { Key => {(numRows, Matrix),(numRows, MutableMatrix),numRows},
     Headline => "number of rows in a matrix or mutable matrix",
     Usage => "numRows m", Inputs => { "m" }, Outputs => {{ "the number of rows in ", TT "m" }}}
document { Key => {(numColumns, Matrix),(numColumns, MutableMatrix),numColumns},
     Headline => "number of columns in a matrix or mutable matrix",
     Usage => "numColumns m", Inputs => { "m" }, Outputs => {{ "the number of columns in ", TT "m" }}}
document { Key => {mutableMatrix,
	  (mutableMatrix, MutableMatrix),
	  (mutableMatrix, Matrix),
	  (mutableMatrix, List),
	  (mutableMatrix, Ring, List),
	  (mutableMatrix, RingFamily, List),
	  [mutableMatrix, Dense]},
     Headline => "make a mutable matrix",
     Usage => "mutableMatrix m",
     Inputs => { "m" => {ofClass{Matrix, MutableMatrix, List}},
	  Dense => {"whether the encoding of the matrix should be dense or not: see ", TO MutableMatrix}
	  },
     Outputs => {{ "a new mutable matrix whose entries are obtained from ", TT "m", ".  If ", TT "m", " is a list, it should
	       be a doubly nested list (table) of ring elements, all from the same ring." }},
     EXAMPLE lines ///
     	  f = mutableMatrix {{1,2,3,4}}
	  f_(0,2)
	  f_(0,2) = 33
	  f
	  R = QQ[a..z]
	  mutableMatrix genericMatrix(R,3,3)
     ///
     }
document { Key => {(mutableMatrix, Ring, ZZ, ZZ),(mutableMatrix, RingFamily, ZZ, ZZ) },
     Headline => "make a mutable matrix filled with zeroes",
     Usage => "mutableMatrix(R,nrows,ncols)",
     Inputs => { "R",
	          "nrows",
		  "ncols",
	  	  Dense => {"whether the encoding of the matrix should be dense or not: see ", TO MutableMatrix}
		  },
     Outputs => {{"an ", TT "nrows", " by ", TT "ncols", " mutable matrix filled with zeroes from the ring ", TT "R" }},
     EXAMPLE lines ///
         m = mutableMatrix(QQ,10,20)
	 m_(5,5) = 11/13
	 m
     ///,
     SeeAlso => {mutableIdentity, mutableMatrix}
     }
document { Key => {(mutableIdentity, Ring, ZZ),(mutableIdentity, RingFamily, ZZ),
	  [mutableIdentity,Dense],
	  mutableIdentity},
     Headline => "make a mutable identity matrix",
     Usage => "mutableIdentity(R,nrows)",
     Inputs => { "R",
	  "nrows",
	  Dense => {"whether the encoding of the matrix should be dense or not: see ", TO MutableMatrix}
	  },
     Outputs => {
	  MutableMatrix => {"an ", TT "nrows", " by ", TT "nrows", " mutable identity matrix filled with elements of the ring ", TT "R" }},
     EXAMPLE lines ///
         m = mutableIdentity(QQ,10)
	 m_(5,5) = 11/13
	 m
     ///,
     SeeAlso => {mutableMatrix}
     }

document { Key => centerString,
     Headline => "center a string or net",
     Usage => "centerString(wid,s)",
     Inputs => { "wid" => ZZ, "s" => Net },
     Outputs => {{"a net with spaces added, as needed, to center ", TT "s", " in a net of width ", TT "wid" }},
     EXAMPLE lines ///
         centerString(18,"asdf"||"qwer")
     ///}

document { Key => {info, (info, Nothing), (info, String)},
     Headline => "convert hypertext to info format",
     "This function is used internally when preparing documentation."
     }
document { Key => pager,
     Headline => "display with paging",
     Usage => "pager x",
     Inputs => {"x"},
     Consequences => {{TT "x", " is converted to a net and displayed through the pager specified by the environment variable PAGER, if set,
	       else through the program ", TT "more", "."
	       }}}

document { Key => {precision,
	  (precision, GaloisField), (precision, FractionField),
	  (precision, QuotientRing), (precision, Ring),(precision,Number),
	  (precision, MutableMatrix),(precision, RingElement),(precision, PolynomialRing),
	  (precision, InexactNumber),(precision, InexactField),(precision, Matrix)
	  },
     Usage => "precision x",
     Inputs => { "x" => {ofClass{Ring,Matrix,RingElement,Number}}},
     Outputs => { ZZ => {"the precision to which ", TT "x", " or its instances are stored"}},
     EXAMPLE lines ///
     	  precision 3p111
	  precision (RR[x])
	  precision 3
     ///
     }

document { Key => "printingTimeLimit",
     "This variable specifies the number of seconds to allow for printing an output line"
     }
document { Key => "printingPrecision",
     Headline => "current precision for printing numbers",
     Usage => "printingPrecision = n",
     Inputs => {
	  "n" => ZZ
	  },
     Consequences => {
	  {"Henceforth, inexact numbers are printed with at most ", TT "n", " digits of precision.
	       Meaningless digits will not be displayed.
	       The special case where ", TT "n=0", " is
	       interpreted as meaning ", TT "n=infinity", ", and this case is
	       used when a number appears alone on an output line to display
	       all the meaningful digits."}
	  },
     EXAMPLE lines ///
     	  1/3p100
     	  {1/3p100}
	  printingPrecision
	  printingPrecision = 16
     	  {1/3p100}
	  printingPrecision = 0
     	  {1/3p100}
     ///,
     PARA {
	  "For complex numbers, if ", TO "printingAccuracy", " is set to its default value of ", TT "-1", ",
	  the two parts of the number are treated together (although a digit further further to the right of
	  the point may sometimes be displayed in the smaller part)."
	  },
     EXAMPLE lines ///
     printingAccuracy
     printingPrecision = 16
     {1p100e12/3+1p100/3*ii}
     printingAccuracy = 10
     {1p100e12/3+1p100/3*ii}
     ///,
     SeeAlso => {"printingAccuracy", "printingLeadLimit", "printingTrailLimit", "printingSeparator", format}
     }
document { Key => "printingAccuracy",
     Headline => "current accuracy for printing numbers",
     Usage => "printingAccuracy = n",
     Inputs => {
	  "n" => ZZ
	  },
     Consequences => {
	  {"Henceforth, inexact numbers are printed with at most ", TT "n", " digits to the right of
	       the decimal point displayed.
	       The special case where ", TT "n=-1", " is
	       interpreted as meaning ", TT "n=infinity", ", and this case is
	       used when a number appears alone on an output line."}
	  },
     EXAMPLE lines ///
	  printingPrecision,printingAccuracy
     	  1p100e-5/3
     	  x = {1p100e-5/3,1p100e-4/3,1p100e-3/3,1p100e-2/3}
	  printingAccuracy = 8
     	  x
	  printingAccuracy = 4
     	  x
     ///,
     SeeAlso => {"printingAccuracy", "printingLeadLimit", "printingTrailLimit", "printingSeparator", format}
     }
document { Key => "printingLeadLimit",
     Headline => "maximum number of leading zeroes to use when printing real numbers",
     Usage => "printingLeadLimit = n",
     Inputs => {
	  "n" => ZZ
	  },
     Consequences => {
	  {"Real numbers are printed with at most ", TT "n", " leading zeroes."}
	  },
     EXAMPLE lines ///
     	  1/30000000000.
	  printingLeadLimit
	  printingLeadLimit = 20
     	  1/30000000000.
     ///,
     SeeAlso => {"printingPrecision", "printingAccuracy", "printingTrailLimit", "printingSeparator", format}
     }
document { Key => "printingTrailLimit",
     Headline => "maximum number of additional trailing digits to use when printing real numbers",
     Usage => "printingTrailLimit = n",
     Inputs => {
	  "n" => ZZ
	  },
     Consequences => {
	  {"Real numbers are printed with at most ", TT "n", " additional trailing digits, in addition to those specified by ", TT "printingPrecision", "."}
	  },
     EXAMPLE lines ///
     	  3000000000000.
	  printingTrailLimit
	  printingTrailLimit = 20
     	  3000000000000.
     ///,
     SeeAlso => {"printingPrecision", "printingAccuracy", "printingLeadLimit", "printingSeparator", format}
     }
document { Key => "printingSeparator",
     Headline => "string used to separate mantissa from exponent when printing real numbers",
     Usage => "printingSeparator = s",
     Inputs => {
	  "s" => String
	  },
     Consequences => {
	  {"The string ", TT "s", " will be used to separate mantissa and exponent when printing real numbers."}
	  },
     EXAMPLE lines ///
     	  3000000000000.
	  printingSeparator
	  printingSeparator = "E"
     	  3000000000000.
     ///,
     SeeAlso => {"printingPrecision", "printingAccuracy", "printingLeadLimit", "printingTrailLimit", format}
     }

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
