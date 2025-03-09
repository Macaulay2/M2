doc ///
  Key
    tally
    (tally, VisibleList)
    (tally, String)
  Headline
    tally the elements of a list, sequence, array, or string
  Usage
    y = tally x
  Inputs
    x:{VisibleList,String}
  Outputs
    y:Tally
  Description
    Text
      It produces a hash table (multiset) @TT "y"@ which tallies the frequencies
      of the occurrences of items in the list or string @TT "x"@, i.e.,
      @TT "y_i"@ is the number of times @TT "i"@ appears in @TT "x"@, or is
      @TT "0"@ if @TT "i"@ doesn't appear in the list or string.
    Example
      y = tally {1,2,3,a,b,1,2,a,1,2,{a,b},{a,b},a}
      y_2
      y_5
      y_{a,b}
      tally "Hello, world!"
  SeeAlso
    Tally
///

document {
     Key => Tally,
     Headline => "the class of all tally results",
     TT "Tally", " -- a class designed to hold tally results, i.e., multisets.",
     SeeAlso => { Set },
     Subnodes => {
	 TO (symbol +, Tally, Tally),
	 TO (symbol -, Tally, Tally),
         }
     }

document {
     Key => VirtualTally,
     "The only difference between this class and ", TO "Tally", " is that this class allows negative numbers.",
     EXAMPLE lines ///
     	  x = tally {a,b,b,c,c,c}
     	  y = tally {a,a,a,b,b,c}
     	  x' = new VirtualTally from x
     	  y' = new VirtualTally from y
	  x-y
	  x'-y'
     ///,
     Subnodes => {
	 TO Tally,
	 TO BettiTally,
	 TO (symbol **, VirtualTally, VirtualTally),
	 TO (symbol ^**, VirtualTally, ZZ),
	 TO (symbol +, VirtualTally, VirtualTally),
	 TO (symbol -, VirtualTally, VirtualTally),
	 TO (symbol -, VirtualTally),
	 TO (symbol _, VirtualTally, Thing),
	 TO (product, VirtualTally),
	 TO (sum, VirtualTally),
         }
     }

document {
     Key => (symbol **, VirtualTally, VirtualTally),
     Headline => "Cartesian product of tallies",
     TT "x ** y", " -- produces the Cartesian product of two tallies.",
     PARA{},
     "One of the arguments may be a ", TO "Set", ".",
     PARA{},
     EXAMPLE {
	  "x = tally {a,a,b}",
      	  "y = tally {1,2,2,2}",
     	  "x ** y",
	  },
     SeeAlso => {"Tally", "tally"}
     }

-- TODO
document {
     Key => (symbol -, VirtualTally, VirtualTally),
}

document {
     Key => (symbol +, VirtualTally, VirtualTally),
     Headline => "union of tallies",
     TT "x + y", " -- produces the union of two tallies.",
     PARA{},
     "One of the arguments may be a ", TO "Set", ".",
     PARA{},
     EXAMPLE {
	      "x = tally {a,b,b,c,c,c,d,d,d}",
      	  "y = tally {a,a,a,b,b,c,d}",
     	  "x' = new VirtualTally from x",
	 	  "y' = new VirtualTally from y",
	 	  "z' = y' - x'",
	 	  "z' + x'",
	  	  "z' + y'",
	  },
     }

document {
     Key => (symbol -, VirtualTally),
     Headline => "negation of a virtual tally",
     TT "-x", " -- the negation of ", TT "x",
     PARA{},
     EXAMPLE {
      	  "x = tally {a,b,b,c,c,d,d,d}",
	 	  "x' = new VirtualTally from x",
	  	  "- x'",
     },
}

document {
     Key => (symbol +, Tally, Tally),
     Headline => "union of tallies",
     TT "x + y", " -- produces the union of two tallies.",
     PARA{},
     "One of the arguments may be a ", TO "Set", ".",
     PARA{},
     EXAMPLE {
	  "x = tally {a,a,a,b,b,c}",
      	  "y = tally {b,c,c,d,d,d}",
      	  "x + y",
	  },
     SeeAlso => {"Tally", "tally"}
     }

document {
     Key => (symbol -, Tally, Tally),
     Headline => "difference of tallies",
     Usage => "x - y",
     Inputs => { "x", "y" },
     Outputs => { { "the difference of the two tallies" } },
     "The count associated to an item ", TT "i", " in the result is the difference of the counts in
     ", TT "x", " and in ", TT "y", " if it's positive, otherwise, zero.",
     EXAMPLE "tally {a,a,b,c} - tally {c,d,d}",
     SeeAlso => "Tally"
     }

document {
     Key => (symbol ^**, VirtualTally, ZZ),
     Headline => "Cartesian power of sets and tallies",
     Usage => "B = A^**n",
     Inputs => { "A", "n" },
     Outputs => {"B" => { "the tally of ", TT "n", "-tuples of elements from ", TT "A" }},
     "If ", TT "A", " is ", ofClass Set, ", then so is ", TT "B", ".",
     EXAMPLE lines ///
     	  A = set {1,2}
	  A^**3
     	  A = tally {1,1,2}
	  A^**3
	  ///,
     SeeAlso => {Set, (symbol**,Set,Set)}
     }

document { 
     Key => (symbol _, VirtualTally, Thing),
     Headline => "get a count from a tally",
     Usage => "t_x",
     Inputs => {
	  "t","x"
	  },
     Outputs => {
	  ZZ => {"the number of times ", TT "x", " is counted by ", TT "t"}
	  },
     EXAMPLE lines ///
     	  t = tally apply(1..10000, i -> # factor i)
	  t_5
	  t_6
	  ///,
     SeeAlso => {Tally}
     }
