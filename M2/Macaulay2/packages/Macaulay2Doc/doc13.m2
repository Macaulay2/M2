--		Copyright 2006 by Daniel R. Grayson


undocumented {(generateAssertions, List)}
document { Key => {generateAssertions,(generateAssertions, String)},
     Headline => "generate assert statements from experimental input",
     Usage => "generateAssertions x",
     Inputs => { "x" => { "a string whose non-comment non-blank lines are Macaulay2 expressions to be evaluated" } },
     Outputs => { { "a net whose lines are assert statements that assert that the expressions evaluate to the expected value, just computed" }},
     EXAMPLE {
	  "generateAssertions ///
2+2
2^20
///",
     	  ///value \ unstack oo///
	  }
     }

document { Key => tutorial,
     Headline => "convert documentation from tutorial format",
     Usage => "tutorial x",
     Inputs => { "x" => String => "documentation in tutorial format" },
     Outputs => {{ "documentation in hypertext format" }},
     PARA { "Some of the Macaulay2 documentation is written in this format." },
     EXAMPLE {
	  "///-- We can compute $(x+y)^3$ as follows.
R = QQ[x,y]
(x+y)^3
///",
     	  "tutorial oo",
	  "peek oo"
	  }}

undocumented {(isConstant, Number)}
document { Key => {isConstant,(isConstant, RingElement)},
     Headline => "whether a ring element is constant",
     Usage => "isConstant f",
     Inputs => { "f" },
     Outputs => { { "whether f is constant, i.e., is in the coefficient ring" } },
     EXAMPLE lines ///
     	  isConstant 3
	  QQ[a,b][x,y];
	  isConstant (x+a-x)
	  isConstant x
	  ///,
     SeeAlso => coefficientRing,
     SourceCode => (isConstant,RingElement)
     }
document { Key => UpdateOnly,
     Headline => "only copies of newer files should replace files" }
document { Key => Verbose,
     Headline => "request verbose feedback" }
document { Key => {ofClass,(ofClass, Type),(ofClass, ImmutableType),(ofClass, List)}, 
     Headline => "English phrases for types",
     Usage => "ofClass T",
     Inputs => { "T" => Nothing => {ofClass{Type,ImmutableType,List}, " of types"} },
     Outputs => { Sequence => { "an English phrase in hypertext, using a synonym for each type, together with appropriate indefinite articles, and, if
	       a list is presented, the word ", EM "or", " as a conjunction at the end" }},
     PARA { "When viewed in html, words in the phrase hot link(s) to the documentation node(s) for the class(es)." },
     EXAMPLE lines ///
     	  ofClass class 3
	  peek oo
     	  ofClass Ring
	  needsPackage "Text"
     	  SPAN ofClass {HashTable, ProjectiveVariety}
	  document { Key => foo, "We may need ", ofClass ZZ, " and ", ofClass HashTable, "." }
	  help foo
     ///}

document { Key => {NetFile,(symbol <<, NetFile, String),(symbol <<, NetFile, Net),(symbol SPACE,Manipulator,NetFile),(symbol <<,NetFile,Manipulator)},
     Headline => "the class of all net files",
     "This class is experimental.  Net files are intended to supplant output files eventually.  Whereas a file is a stream of bytes,
     or in some non-unix operating systems, a sequence of lines each of which is a sequence of bytes, a net file is a sequence of lines, each of which is
     a net.  Each output line is assembled by joining nets one by one.",
     EXAMPLE lines ///
     	  f = newNetFile()
     	  f << "aabbcc" << endl
	  f << "aa" << "bb"^1 << "cc"^-1 << endl
	  f << "aa" << "bb"^1 << "cc"^-1 << endl
     	  getNetFile f
	  peek oo
     	  class \ ooo
     ///
     }
document { Key => getNetFile,
     Headline => "get the sequence of completed lines (nets) from a net file",
     Usage => "getNetFile n",
     Inputs => { "n" => NetFile },
     "This function is experimental."
     }
document { Key => newNetFile,
     Headline => "create a new net file",
     Usage => "newNetFile()",
     Outputs => { NetFile },
     "This function is experimental."
     }
document { Key => zero,
     Headline => "whether something is zero",
     SourceCode => zero,
     Usage => "zero x",
     Inputs => { "x" },
     Outputs => { { "whether ", TT "x", " is equal to 0" }}}
document { Key => {Bag,(symbol ?,Bag,Bag)},
     Headline => "the class of all bags",
     PARA "A bag can be used for enclosing something in a container to prevent it from being printed, in normal circumstances.
     Any mutable list can be used for this purpose, but bags are designed for this purpose.  In comparison and sorting, they
     are declared to be incomparable.",
     SeeAlso => {unbag}
     }
document { Key => {(unbag, Bag), unbag},
     Usage => "unbag y",
     Inputs => { "y" },
     Outputs => { { "the contents of ", TT "y" }},
     EXAMPLE lines ///
     	  x = 100!
	  y = Bag {x}
	  unbag y
     ///
     }

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
