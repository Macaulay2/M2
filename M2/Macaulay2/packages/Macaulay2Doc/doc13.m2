--		Copyright 2006 by Daniel R. Grayson

document {
     Key => cacheValue,
     Headline => "cache values of functions in their arguments",
     Usage => "((cacheValue KEY) f) x",
     Inputs => {
	  "KEY",
	  "f" => Function,
	  "x" => {"an argument for ", TT "f", " that has ", ofClass CacheTable, " stored in it under ", TT "x.cache"}
	  },
     Outputs => {
	  { TT "f x", " is returned, but the value is saved in ", TT "x.cache#KEY", " and not recomputed later.
	       However, if the value found in ", TT "x.cache#KEY", " is ", ofClass CacheFunction, ", such as is
	       returned by ", TT "(stashValue KEY) f", ", then the value of ", TT "x.cache#KEY x", " is returned instead, after
	       first removing ", TT "x.cache#KEY", " from ", TT "x.cache", "." }
	  },
     EXAMPLE {
	  "x = new HashTable from { val => 1000, cache => new CacheTable }",
	  ///f = (t -> (print "hi there"; t.val^4))///,
	  ///h = (cacheValue VALUE) f///,
	  "h x",
	  "h x",
	  "peek'_2 x"
	  },
     SourceCode => { cacheValue },
     SeeAlso => { stashValue }
     }

document {
     Key => stashValue,
     Headline => "stash values of functions in their arguments",
     Usage => "((stashValue KEY) f) x",
     Inputs => {
	  "KEY",
	  "f" => Function,
	  "x" => MutableHashTable => { "an argument for ", TT "f" }
	  },
     Outputs => {
	  { "The value of ", TT "f x", " is returned, but the value is saved in ", TT "x#KEY", " and not recomputed later.
	       However, if the value found in ", TT "x#KEY", " is ", ofClass CacheFunction, ", such as is
	       returned by ", TT "(stashValue KEY) f", ", then the value of ", TT "x#KEY x", " is returned instead, after
	       first removing ", TT "x#KEY", " from ", TT "x", "."
	       }
	  },
     EXAMPLE {
	  "x = new MutableHashTable from { val => 1000 }",
	  ///f = (t -> (print "hi there"; t.val^4))///,
	  ///h = (stashValue VALUE) f///,
	  "h x",
	  "h x",
	  "peek x"
	  },
     SourceCode => { stashValue },
     SeeAlso => { cacheValue }
     }

document {
     Key => CacheFunction,
     Headline => "the class of cache functions",
     "Functions of class ", TO "CacheFunction", " are created and used by ", TO "cacheValue", " and by ", TO "stashValue", "."
     }

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
document { Key => unsequence,
     Headline => "extract the single element from a sequence of length 1",
     Usage => "unsequence x",
     Inputs => { "x" => Thing },
     Outputs => { { TT "x#0", ", if ", TT "x", " is a sequence of length 1, otherwise ", TT "x", "" } },
     EXAMPLE { "unsequence (2:a)", "unsequence (1:a)", "unsequence (0:a)" },
     SeeAlso => sequence}

document { Key => {permutations, (permutations, ZZ), (permutations, VisibleList)},
     Headline => "produce all permutations of a list",
     Usage => "permutations x",
     Inputs => { "x" => { ofClass {VisibleList, ZZ} } },
     Outputs => { { "a list of all the permutations of the visible list ", TT "x", ", or, if ", TT "x", " is an integer, of the list of
	       integers from 0 through ", TT "n-1" 
	       } },
     EXAMPLE {
	  "permutations {a,b,c,d}",
	  "permutations 3"
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
document { Key => {preimage,(preimage, RingMap, Ideal)},
     Headline => "preimage of an ideal under a ring map",   -- hopefully more general later
     Usage => "preimage(f,I)",
     Inputs => { "I" => { "an ideal in the target ring of ", TT "f" }, "f" },
     Outputs => { { "the preimage of ", TT "I", " under the map ", TT "f" } },
     EXAMPLE lines /// 
	  R = QQ[x,y,z]
	  S = QQ[t,u]
	  f = map(R,S,{x*y,y*z})
	  preimage_f ideal(x^2,y^2)
     	  ///
     }
document { Key => {round,(round,QQ),(round,RR),(round,ZZ,RR),(round,ZZ),(round,CC)},
     Headline => "round a number",
     SYNOPSIS (
	  Usage => "round x",
	  Inputs => { "x" => "a number" },
	  Outputs => {{ "the integer nearest to ", TT "x" }},
	  EXAMPLE lines ///
	  round(-2.3)
	  round(-2.3+5*ii)
	  round(2/3)
	  ///
	  ),
     SYNOPSIS (
	  Usage => "round(n,x)",
	  Inputs => { "n" => ZZ, "x" => RR },
	  Outputs => {{ "the real number with just n decimal digits to the right of the decimal point nearest to ", TT "x" }},
	  EXAMPLE lines ///
	  round(2,1234.5678)
	  round(-2,1234.5678)
	  ///
	  ),
     SeeAlso => { floor, ceiling }
     }

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
document { Key => Partition,
     Headline => "a type of list representing a partition of a natural number",
     SeeAlso => { partitions, (conjugate,Partition) } }
document { Key => (conjugate,Partition),
     Headline => "conjugate a partition",
     Usage => "conjugate p", Inputs => {"p"}, Outputs => {{"the conjugate of ", TT "p" }},
     EXAMPLE lines ///
     	  partitions 4
	  conjugate \ oo
     ///}
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

document { Key => VerticalList,
     Headline => "a type of visible self-initializing list that prints vertically",
     Usage => "VerticalList x",
     Inputs => { "x" => List },
     Outputs => { VerticalList },
     "All operations on lists apply to vertical lists, since they inherit from the type ", TO VisibleList, ".  The
     only difference is the way that a vertical list is displayed vertically.",
     EXAMPLE lines ///
     	 a .. e
	 v = VerticalList oo
     	 v_1
	 length v
	 ///,
     "One may get a normal list back from a vertical list as follows.",
     EXAMPLE lines ///
     	 toList v
         ///,
     SeeAlso => { NumberedVerticalList }
     }

document { Key => NumberedVerticalList,
     Headline => "a type of visible self-initializing list that prints vertically",
     Usage => "NumberedVerticalList x",
     Inputs => { "x" => List },
     Outputs => { NumberedVerticalList },
     "All operations on lists apply to numbered vertical lists, since they inherit from the type ", TO VisibleList, ".  The
     only difference is the way that a numbered vertical list is displayed vertically, with index numbers labelling the entries.",
     EXAMPLE lines ///
     	 a .. e
	 v = NumberedVerticalList oo
     	 v_1
	 length v
	 ///,
     "One may get a normal list back from a vertical list as follows.",
     EXAMPLE lines ///
     	 toList v
         ///,
     SeeAlso => { VerticalList }
     }


document { Key => {LowerBound,(symbol SPACE,CoherentSheaf,LowerBound),(symbol SPACE,SheafOfRings,LowerBound)},
     Headline => "the class of lower bound objects",
     "This is a type of list that represents a lower bound.  The single element of the list is an integer, and the object represents the condititon
     that some other integer, such as the index in a direct sum, should be at least as large.",
     EXAMPLE {
	  "LowerBound {4}",
	  ">= 4",
	  "> 4"
	  }}

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
document { Key => SheafOfRings,
     SeeAlso => { Variety, OO },
     Headline => "the class of sheaves of rings",
     EXAMPLE lines ///
     	  X = Proj(QQ[x..z])
	  OO_X
	  OO_X^6
     ///
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
