--		Copyright 1993-1998 by Daniel R. Grayson

---------------------
---- Operators ------
---------------------
document {
     Key => symbol <<, 
     Headline => "a binary operator (file output, ...)",
     }
document {
     Key => {"left shift", (symbol <<, ZZ, ZZ), (symbol <<, RR, ZZ), (symbol <<, CC, ZZ)},
     Usage => "x << j",
     Inputs => { "x", "j" },
     Outputs => {{ "the number obtained from ", TT "x", " by shifting its binary representation leftward ", TT "j", " places" }},
     EXAMPLE {"256 << 5","256. << 555"},
     SeeAlso => {"right shift"}
     }

document {
     Key => {"right shift", (symbol >>, ZZ, ZZ), (symbol >>, RR, ZZ), (symbol >>, CC, ZZ)},
     Usage => "x >> j",
     Inputs => { "x", "j" },
     Outputs => {{ "the integer obtained from ", TT "x", " by shifting its binary representation rightward ", TT "j", " places" }},
     EXAMPLE {"256 >> 5","256. >> 555"},
     SeeAlso => {"left shift"}
     }

document {
     Key => { (symbol <<, File, Thing),(symbol <<, String, Thing), (symbol <<, File, Manipulator),(symbol <<, List, Thing),
	  (symbol <<, Nothing, Thing),(symbol <<, Nothing, Manipulator), (symbol <<, Thing),
	  (symbol <<, File, Symbol),(symbol <<, File, Net),(symbol <<,File,String) },
     Headline => "print to a file",
     Usage => "f << x\n  << x",
     Inputs => { 
	  "f" => Nothing => { ofClass {File, String, List, Nothing} },
	  "x"
	  },
     Outputs => {
	  File => "the output file(s) used"
     	  },
     Consequences => {{
	  "The object ", TT "x", " is prepared for printing (with ", TO "net", ") and printed on the output file(s) ", TT "f", ".
	  If ", TT "f", " is a string, then it is interpreted as a filename and an output file is opened, used, and returned,
	  unless a single open file with the same name already exists, in which case it is used and returned.
	  Filenames starting with ", TT "!", " or with ", TT "$", " are treated specially, see ", TO "openInOut", ".
	  If ", TT "f", " is a list, then the output operation is performed on each one.
	  If ", TT "f", " is ", TO "null", ", then the output is discarded; thus ", TO "null", " is useful as a dummy output file.
	  If ", TT "f", " is omitted, as in the second usage line, then the output is sent to ", TO "stdio", ", and it will appear (usually) on the screen."
	  }},
     PARA {
	  "Parsing of ", TO "<<", " associates leftward, so that several objects  may be displayed with an expression such as ", TT "f<<x<<y<<z", "."
	  },
     EXAMPLE lines ///
     	  stderr << "-- hi there --" << endl
     	  << "-- ho there --" << endl
	  fn = temporaryFileName()
	  fn << "hi there" << endl << close
	  get fn
	  R = QQ[x]
	  f = (x+1)^10
	  << f
	  fn << f << close
     	  get fn
	  fn << toExternalString f << close
     	  get fn
	  value get fn
	  removeFile fn
     ///,
     SeeAlso => { stdio, stderr, endl, close }
     }

document {
     Key => symbol >>, 
     Headline => "a binary operator, uses include bit shifting, or attaching optional inputs to functions" 
     }

document {
     Key => symbol :,
     Headline => "a binary operator, uses include repetition; ideal quotients",
     }
document {
     Key => symbol <,
     Headline => "less than",
     TT "x < y", " yields ", TO "true", " or ", TO "false", 
     " depending on whether x < y.",
     PARA{},
     "Calls upon ", TO "?", " to perform the comparison, if necessary."
     }
document {
     Key => symbol <=,
     Headline => "less than or equal",
     TT "x <= y", " yields ", TO "true", " or ", 
     TO "false", " depending on whether x <= y.",
     PARA{},
     "Calls upon ", TO "?", " to perform the comparison, if necessary."
     }
document {
     Key => symbol >,
     Headline => "greater than",
     TT "x > y", " yields ", TO "true", " or ", 
     TO "false", " depending on whether x > y.",
     PARA{},
     "Calls upon ", TO "?", " to perform the comparison, if necessary."
     }
document {
     Key => symbol >=,
     Headline => "greater than or equal",
     Usage => "x >= y",
     Inputs => {"x", "y"},
     Outputs => {
	  Boolean => "depending on whether x >= y"
	  },
     "Calls upon ", TO "?", " to perform the comparison, if necessary.",
     }
undocumented {
     (symbol >, Thing, Thing),
     (symbol <, Thing, Thing),
     (symbol <=, Thing, Thing),
     (symbol >=, Thing, Thing)
     }

undocumented {
     (symbol ?, TO2, TO2),
     (symbol ?, TO2, TO),
     (symbol ?, TO, TO2),
     (symbol ?, TOH, TO2),
     (symbol ?, TO2, TOH),
     (symbol ?, TO, TO),
     (symbol ?, String, DocumentTag),
     (symbol ?, DocumentTag, String),
     (symbol ?, TOH, TO),
     (symbol ?, TO, TOH),
     (symbol ?, InfiniteNumber, InfiniteNumber),
     (symbol ?, DocumentTag, DocumentTag),
     (symbol ?, Thing, InfiniteNumber),
     (symbol ?, TOH, TOH),
     (symbol ?, InfiniteNumber, Thing),
     (symbol ?, ZZ, MonoidElement),
     (symbol ?, MonoidElement, ZZ),
     (symbol ?, RingElement, ZZ),
     (symbol ?, ZZ, RingElement)
     }

document {
     Key => {symbol ?,
     	  (symbol ?, Symbol, IndexedVariable),
     	  (symbol ?, IndexedVariable, IndexedVariable),
     	  (symbol ?, List, List),
	  (symbol ?, VirtualTally, VirtualTally)
	  },
     Headline => "comparison operator",
     Usage => "x ? y", 
     Inputs => { "x", "y" },
     Outputs => {{
	  "One of the symbols ", TT "symbol <", ", ", TT "symbol >", ", ", TT "symbol ==", ", or ", TT "incomparable", ",
	  depending (respectively) on whether ", TT "x < y", ", ", TT "x > y", ", ", TT "x == y", ", or ", TT "x", " and ", TT "y", " are not comparable."	  
	  }},
     "Many types of objects may be compared.  Numbers are handled as one would expect,
     and strings, lists and sequences are generally compared lexicographically.",
     EXAMPLE lines ///
     	  3 ? 4
	  "book" ? "boolean"
	  3 ? 3.
	  {1,2,3} ? {4,5}
     ///,
     PARA{},
     "Polynomials from the same ring may also be compared.  The order depends on the 
     monomial order in the ring.",
     EXAMPLE lines ///
     	  R = ZZ[a,b,c]
	  a*c ? b^2
     ///,
     "A set is smaller than another if it is a subset; for tallies, corresponding counts should all be smaller.",
     EXAMPLE lines ///
     	  set {1,2} ? set {2,3}
     	  set {1,2} ? set {1,2,3}
	  tally {1,1,2} ? tally {1,2,3}
	  tally {1,1,2} ? tally {1,1,2,3}
     ///,
     SeeAlso => {sort, rsort}
     }

protect incomparable
document {
     Key => incomparable,
     Headline => "a result indicating incomparability",
     TT "incomparable", " a symbol that may be returned by ", TO "?", "
     when the two things being compared are incomparable."
     }

doc ///
  Key
    (lcm,MonomialIdeal)
  Headline
    least common multiple of all minimal generators
  Usage
    m = lcm I
  Inputs
    I:MonomialIdeal
  Outputs
    m:RingElement
  Description
   Text
     This function is implemented in the engine, as it is used in many algorithms involving monomial ideals.
   Example
     R = QQ[a..d];
     I = monomialIdeal "a4,a3b6,a2b8c2,c4d5"
     lcm I
     first exponents lcm I
  SeeAlso
    (dual,MonomialIdeal)
    "PrimaryDecomposition::irreducibleDecomposition(MonomialIdeal)"
    "PrimaryDecomposition::primaryDecomposition(Ideal)"
///

document {
     Key => {concatenate,(concatenate, ZZ), (concatenate, BasicList), (concatenate, String), (concatenate, Nothing), (concatenate, Symbol)},
     Headline => "join strings",
     TT "concatenate(s,t,...,u)", " yields the concatenation of the strings s,t,...,u.",
     PARA{},
     "The arguments may also be lists or sequences of strings and symbols, in
     which case they are concatenated recursively.  Additionally,
     an integer may be used to represent a number of spaces, and ", TO "null", " will be represented by the empty string.",
     EXAMPLE ///concatenate {"a",("s",3,"d",),"f"}///,
     SeeAlso => { "String"} 
     }

document {
     Key => symbol ~,
     Headline => "a unary postfix operator",
     }

document {
     Key => {mergePairs,(mergePairs, BasicList, BasicList, Function)},
     Headline => "merge sorted lists of pairs",
     TT "mergePairs(x,y,f)", " merges sorted lists of pairs.",
     PARA{},
     "It merges ", TT "x", " and ", TT "y", ", which should be lists 
     of pairs ", TT "(k,v)", " arranged in increasing order according
     to the key ", TT "k", ".  The result will be a list of pairs, also
     arranged in increasing order, each of which is either from ", TT "x", "
     or from ", TT "y", ", or in the case where a key ", TT "k", " occurs in
     both, with say ", TT "(k,v)", " in ", TT "x", " and ", TT "(k,w)", "
     in ", TT "y", ", then the result will contain the pair ", TT "(k,f(v,w))", ".
     Thus the function ", TT "f", " is used for combining the values when the keys
     collide.  The class of the result is taken to be the minimal common
     ancestor of the class of ", TT "x", " and the class of ", TT "y", ".",
     PARA{},
     SeeAlso => { "merge" }
     }


document {
     Key => combine,
     Headline => "combine hash tables",
     Usage => "z = combine(x,y,f,g,h)",
     Inputs => {
	  "x" => "a hash table",
	  "y" => {"a hash table of the same class as ", TT "x"},
	  "f" => { "a function of two variables to be used for combining a key
	       of ", TT "x", " with a key of ", TT "y", " to make a new key
	       for ", TT "z", "." },
	  "g" => { "a function of two variables to be used for combining a value
	       of ", TT "x", " with a value of ", TT "y", " to make a new value
	       for ", TT "z", "." },
	  "h" => { "a function of two variables to be used for combining two
	       values returned by ", TT "g", " when the corresponding keys
	       returned by ", TT "f", " turn out to be equal.  Its first argument
	       will be the value accumulated so far, and its second argument will
	       be a value just provided by ", TT "g", "."
	       }
	  },
     Outputs => {
	  "z" => {
	       "a new hash table, of the same class as ", TT "x", " and ", TT "y", ",
	       containing the pair ", TT "f(p,q) => g(b,c)", "
	       whenever ", TT "x", " contains the pair ", TT "p => b", "
	       and ", TT "y", " contains the pair ", TT "q => c", ",
	       except that ", TT "h", " is used to combine values when two keys
	       coincide.  If ", TT "f", " or ", TT "g", " evaluates ", TO "continue", ", then
	       nothing is contributed to the resulting hash table.  If ", TT "h", " evaluates
	       ", TO "continue", ", then, at that point, the entry stored under the key ", TT "f(p,q)", " 
	       in the hash table under construction is removed."
	       }
	  },
     "The function ", TT "f", " is applied to every pair ", TT "(p,q)", "
     where ", TT "p", " is a key of ", TT "x", " and ", TT "q", " is a
     key of ", TT "y", ".  The number of times ", TT "f", " is evaluated is thus 
     the product of the number of keys in ", TT "x", " and the number of 
     keys in ", TT "y", ".",
     PARA{},
     "The function ", TT "h", " should be an associative function, for otherwise 
     the result may depend on internal details about the implementation of hash 
     tables that affect the order in which entries are encountered.  If ", TT "f", ",
     ", TT "g", ", and ", TT "h", " are commutative functions as well, then the 
     result ", TT "z", " is a commutative function of ", TT "x", " and ", TT "y", ".",
     PARA{},
     "The result is mutable if and only if ", TT "x", " or ", TT "y", " is.",
     PARA{},
     "This function can be used for multiplying polynomials, where it
     can be used in code something like this:", 
     PRE "     combine(x, y, monomialTimes, coeffTimes, coeffPlus)",
     "We illustrate that with a simple-minded implmentation of the free ring on the English alphabet, representing words
     as string and polynomials as hash tables that associate coefficients to words.",
     EXAMPLE lines ///
     	  Poly = new Type of HashTable
     	  p = new Poly from { "" => 1, "x" => 2, "y" => 3, "cat" => 5 }
	  Poly * Poly := (p,q) -> combine(p,q,concatenate,times,plus);
	  p*p
     ///,
     SeeAlso => {merge}
     }

document {
     Key => {symbol =>,(symbol =>, Thing, Thing)},
     Headline => "produce an Option",
     TT "x => y", " a binary operator that produces a type of list called
     an ", TO "Option", "."
     }     

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
