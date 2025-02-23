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
     SeeAlso => {sort, rsort, (symbol?, Symbol)}
     }

protect incomparable
document {
     Key => incomparable,
     Headline => "a result indicating incomparability",
     TT "incomparable", " a symbol that may be returned by ", TO "?", "
     when the two things being compared are incomparable."
     }
