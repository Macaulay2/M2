--		Copyright 2006 by Daniel R. Grayson

document {
     Key => "lists and sequences",
     "In this section we give an overview of the use of lists of all types, including:",
     UL {
	  SPAN {"basic lists (of class ", TO "BasicList", ")," },
	  SPAN {"visible lists (of class ", TO "VisibleList", ")," },
	  SPAN {"lists (of class ", TO "List", ")," },
	  SPAN {"sequences (of class ", TO "Sequence", "), and" },
	  SPAN {"arrays (of class ", TO "Array", ")." },
	  SPAN {"mutable lists (of class ", TO "MutableList", ")." },
	  },
     SUBSECTION "lists",
     "A list is a handy way to store a series of things.  We create one
     by separating the elements of the series by commas and surrounding 
     the series with braces.",
     EXAMPLE "x = {a,b,c,d,e}",
     "We retrieve the length of a list with the operator ", TO "#", " or with the function ", TT "length", ".",
     EXAMPLE {"#x","length x"},
     "We use the expression ", TT "x#n", " to obtain the n-th element of ", TT "x", ".  The elements are numbered consecutively starting with ", TT "0", ".
     Alternatively, they are numbered consecutively ending with ", TT "-1", ".",
     EXAMPLE {"x#2","x#-2"},
     "The functions ", TO "first", " and ", TO "last", " retrieve the first and last elements of a list.",
     EXAMPLE lines ///
          first x
	  last x
     ///,
     PARA {
	  "Omitting an element of a list causes the symbol ", TO "null", " to 
	  be inserted in its place."
	  },
     EXAMPLE lines (///
      	  g = {3,4,,5}
       	  peek g
     ///),
     PARA {
	  "Lists can be used as vectors, provided their elements are the sorts of
	  things that can be added and mutliplied."},
     EXAMPLE "10000*{3,4,5} + {1,2,3}",
     "If the elements of a list are themselves lists, we say that we have
     a nested list.",
     EXAMPLE {
	  "y = {{a,b,c},{d,{e,f}}}",
	  "#y"
	  },
     "One level of nesting may be eliminated with ", TO "flatten", ".",
     EXAMPLE "flatten y",
     "A table is a list whose elements are lists all of the same length.  
     The inner lists are regarded as rows when the table is displayed as a
     two-dimensional array with ", TO "MatrixExpression", ".",
     EXAMPLE lines ///
	  z = {{a,1},{b,2},{c,3}}
	  isTable z
      	  MatrixExpression z
     ///,
     SUBSECTION "sequences",
     "Sequence are like lists, except that parentheses are used instead of braces to create them and to print them.  Sequences
     are implemented in a more efficient way than lists, since a sequence is created every time a function is called with more than one argument.  
     Another difference is that new types of list can be created by the user, but not new types of sequence.",
     EXAMPLE lines ///
          x = (a,b,c,d,e)
	  #x
	  x#2
     ///,
     "It is a bit harder to create a sequence of length 1, since no comma
     would be involved, and parentheses are also used for simple grouping
     of algebraic expressions.",
     EXAMPLE lines ///
          ()
	  (a)
	  (a,b)
     ///,
     "Most of the functions that apply to lists also work with sequences.  We
     give just one example.",
     EXAMPLE "append(x,f)",
     "The functions ", TO "toList", " and ", TO "toSequence", " are provided
     for converting between lists to sequences.",
     EXAMPLE {
	  "toList x",
	  "toSequence oo",
	  },
     "Other functions for dealing especially with sequences
     include ", TO "sequence", " and ", TO "deepSplice", ".",
     SUBSECTION "arrays",
     "An array is like a list, except that brackets are used instead of
     braces when entering or displaying an array, and arrays can't be used
     as vectors.  Their main use is notational: for example, they appear
     in the construction of polynomial rings.",
     EXAMPLE {
	  "v = [a,b,c]",
	  "v#2",
	  "ZZ[a,b,c]"
	  },
     SUBSECTION "visible lists",
     "Lists, sequences, and arrays are the three examples of what we call visible lists, which constitute the class ", TO "VisibleList", ".  Many functions
     are defined to act uniformly on visible lists.",
     EXAMPLE lines ( ///
     	  {a,b,c}
     	  class oo
     	  parent oo
     /// ),
     SUBSECTION "basic lists",
     "There is a type of list more general than a visible list, which we call a basic list.  Basic lists can be used for representing new datatypes
     in a more secure way, since the many functions that act on lists and sequences do not act on basic lists.",
     EXAMPLE lines ( ///
     	  {a,b,c}
     	  class oo
     	  parent oo
     	  parent oo
     /// ),
     "We can make a basic list with the ", TO "new", " operator.",
     EXAMPLE "new BasicList from {a,b,c}",
     "Similarly, we can make a new type of basic list, called ", TT "Container", ", say.",
     EXAMPLE "Container = new Type of BasicList",
     "We can make a new list of type Container.",
     EXAMPLE "t = new Container from {a,b}",
     "Some functions work on basic lists.",
     EXAMPLE "join(t,t)",
     "We can make a new method for the operator ", TT "++", ", say, that will join two such lists.",
     EXAMPLE lines ///
	 Container ++ Container := join;
	 t ++ t
     ///,
     SUBSECTION "mutable lists",
     "The elements of a basic list cannot normally be replaced by others.  However, there is a certain type of
     basic list, called a mutable list (of class ", TO "MutableList", "), whose elements can be changed.  Because
     the elements of a mutable list can be changed, circular structures can be created that would cause a print
     routine to go into an infinite loop.  We avoid such infinite loops by not printing out the contents of mutable 
     lists.  Instead, one uses ", TO "peek", " to display the elements in a controlled way.",
     EXAMPLE lines ///
	  s = new MutableList from {a,b,c}
      	  peek s
      	  s#2 = 1234;
	  s
      	  peek s
     ///,
     "Because the contents of mutable lists are not printed, they can be used as containers for big things that one
     normally doesn't want printed.  For this purpose we have a special type of mutable list called a bag (of class ", TO "Bag", "),
     that displays, when printed, a little information about its contents.",
     EXAMPLE lines ///
     	  Bag {100!}
	  peek oo
     ///,     
     SUBSECTION "summary",
     "We can see the hierarchy of types mentioned above using ", TO "showStructure", ".",
     EXAMPLE lines ///
        showStructure(List,Sequence,Array,Container,MutableList,Bag,BasicList)
     ///,
     Subnodes => {
	  TO "ranges and repetitions",
     	  "basic access methods",
	  TO (symbol #, BasicList),
	  TO (symbol #, BasicList, ZZ),
	  TO (symbol #?, BasicList, ZZ),
	  TO (symbol _, VisibleList, ZZ),
	  TO (symbol _, VisibleList, List),
	  TO first,
	  TO last,
	  "Conversions",
	  TO toList,
	  TO toSequence,
	  TO sequence,
	  TO unsequence,
     	  "manipulating lists and sequences",
	  TO append,
	  TO between,
	  TO delete,
	  TO drop,
	  TO flatten,
	  TO fold,
	  TO join,
	  TO (symbol|,List,List),
	  TO mingle,
	  TO pack,
	  TO prepend,
	  TO reverse,
	  TO rsort,
	  TO sort,
	  TO subtable,
	  TO table,
	  TO take,
	  TO unique,
     	  "applying functions to elements of lists",
	  TO (apply,BasicList,Function),
	  TO (scan,BasicList,Function),
     	  "testing elements of lists",
	  TO (all,BasicList,Function),
	  TO (any,BasicList,Function),
     	  "finding things in lists",
	  TO (position,VisibleList,Function),
	  TO (positions,VisibleList,Function),
	  TO (select,BasicList,Function),
	  TO (select,ZZ,BasicList,Function),
	  "more information",
	  TO VisibleList,
	  TO BasicList
	  }
     }

document {
     Key => "ranges and repetitions",
     PARA {
	  "In this section we discuss the use of ranges and repetitions."
	  },
     SUBSECTION "ranges",
     "The operator ", TO "..", " can be used to create sequences of numbers,
     sequences of subscripted variables, or sequences of those particular 
     symbols that are known to ", TO "vars", ", and so on.",
     EXAMPLE lines ///
	  1 .. 5, y_1 .. y_5, a .. e
     ///,
     SUBSECTION "repetitions",
     "The operator ", TO (symbol :, ZZ, Thing), " is used to create sequences by replicating something a certain number of times.",
     EXAMPLE "12:a",
     "Replicating something once results in a sequence of length 1, which cannot be entered by simply typing parentheses.",
     EXAMPLE { "1:a", "(a)" },
     SUBSECTION "ranges and repetitions in lists",
     "Notice what happens when we try to construct a list using ", TO "..", " or ", TO ":", ".",
     EXAMPLE {
	  "z = {3 .. 6, 9, 3:12}",
	  },
     "The result above is a list of length 3 some of whose elements are sequences.
     This may be a problem if the user intended to produce the list 
     ", TT "{3, 4, 5, 6, 9, 12, 12, 12}", ".  The function ", TO "splice", " can
     be used to flatten out one level of nesting - think of it as removing those
     pairs of parentheses that are one level inward.",
     EXAMPLE "splice z",
     "The difference between ", TO "splice", " and ", TO "flatten", " is, essentially, that
     ", TO "flatten", " removes braces one level inward.",
     EXAMPLE lines ///
         flatten {a,{b,c}}
         splice {a,(b,c)}
     ///,
     "The function ", TO "toList", " converts sequences to lists.",
     EXAMPLE lines ///
          1..6
          toList(1..6)
     ///,
     "Many operators and functions will splice lists presented to them.  For example, when
     creating a polynomial ring, the array of variables and the list of degrees are spliced for you.",
     EXAMPLE lines ///
         QQ[a..c,x_1..x_4, Degrees => { 3:1, 4:2 }]
	 degrees oo
     ///
     }

document {
     Key => BasicList,
     Headline => "the class of all basic lists",
     PARA {"For an overview of lists and sequences, see ", TO "lists and sequences", "."},
     "A basic list is a sequence of expressions indexed by a seequence of consecutive integers of the form
     ", TT "0", ", ", TT "1", ", ..., ", TT "N-1", ".  The number ", TT "N", " is called the length of the list.",
     PARA{},
     "There are various types of basic lists, depending on the application, and they are displayed in different ways.
     The types first encountered are those of type ", TO "VisibleList", ", but new types are easy to introduce.
     In the following example we introduce a new type of basic list called ", TT "L", ".",
     EXAMPLE {
	  "L = new Type of BasicList",
	  "x = new L from {a,b,c,d}",
	  "join(x,x)"
	  }
     }

document {
     Key => VisibleList,
     Headline => "the class of all visible lists",
     "There are three types of lists that can be entered directly from
     the keyboard, as follows.",
     EXAMPLE {
	  "{a,b,c}",
	  "[a,b,c]",
	  "(a,b,c)",
	  },
     "We introduce the class of visible lists as a convenience for
     referring to lists of these types.",
     Subnodes => {
	  TO List,
	  TO Sequence,
	  TO Array
	  }
     }

document {
     Key => List,
     Headline => "the class of all lists -- {...}",
     "Lists in Macaulay2 consist of elements of any type, enclosed in braces, and separated by commas.",
     EXAMPLE "L = {a,1,b,2}",
     "The length of a list has two notations, the version with the ",  TT "#", " is faster when writing programs.",
     EXAMPLE "#L, length L",
     "The first entry of the list has index 0.  Indexing is performed using ", TO symbol#, ".",
     EXAMPLE "L#2",
     PARA{
	  "Lists in Macaulay2 are immutable. See ", TO MutableList, " for making and using lists that you may modify."
	  },
     PARA {
	  "To convert lists to and from other types of ", TO "BasicList", ", in addition to ", TO "toList", ", one may use ", TO "new", "."
	  },
     EXAMPLE lines ///
     new Array from {a,b,c}
     new List from [a,b,c]
     ///,
     PARA {"For an overview of lists and sequences, see ", TO "lists and sequences", "."},
     }

document {
     Key => Array,
     Headline => "the class of all arrays -- [...]",
     PARA {
	  "An array can be created by enclosing elements of any type in brackets."
	  },
     EXAMPLE lines ///
     x = [a,b,c]
     # x
     x#1
     ///,
     PARA {
	  "To convert arrays to and from other types of ", TO "BasicList", ", one may use ", TO "new", "."
	  },
     EXAMPLE lines ///
     new Array from {a,b,c}
     new Sequence from [a,b,c]
     ///,
     PARA {"For an overview of lists and sequences, see ", TO "lists and sequences", "."}
     }

document {
     Key => Sequence,
     Headline => "the class of all sequences -- (...)",
     PARA {
	  "A sequence is an ordered collection of things enclosed by parentheses
	  and separated by commas.  Use ", TO "#", " to get the length of a
	  sequence of to get one of the elements."
	  },
     EXAMPLE {
	  "v = (a,b,c)",
	  "#v",
	  "v#2"
	  },
     PARA {
	  "To convert sequences to and from other types of ", TO "BasicList", ", in addition to ", TO "toSequence", ", one may use ", TO "new", "."
	  },
     EXAMPLE lines ///
     new Array from (a,b,c)
     new Sequence from [a,b,c]
     ///,
     PARA {"For an overview of lists and sequences, see ", TO "lists and sequences", "."},
     }

document {
     Key => (symbol :, ZZ, Thing),
     Headline => "repeat an item",
     TT "n : x", " repetition ", TT "n", " times of ", TT "x", " in a sequence",
     PARA{},
     "If ", TT "n", " is an integer and ", TT "x", " is anything, return a
     sequence consisting of ", TT "x", " repeated ", TT "n", " times.  A negative 
     value for ", TT "n", " will silently be treated as zero.",
     PARA{},
     "Warning: such sequences do not get automatically spliced into lists
     containing them.",
     PARA{},
     EXAMPLE { "{5:a,10:b}", "splice {5:a,10:b}" },
     SeeAlso => {splice, (symbol..,ZZ,ZZ), "ranges and repetitions"}
     }

document {
     Key => {toSequence,(toSequence, BasicList)},
     Headline => "convert to sequence",
     TT "toSequence x", " -- yields the elements of a list ", TT "x", " as a sequence.",
     PARA{},
     "If ", TT "x", " is a sequence, then ", TT "x", " is returned.",
     PARA{},
     EXAMPLE {
	  "toSequence {1,2,3}"
	  },
     }

undocumented (deepSplice,BasicList)
document {
     Key => deepSplice,
     Headline => "remove subsequences",
     TT "deepSplice v", " -- yields a new list v where any members of v 
     which are sequences are replaced by their elements, and so on.",
     PARA{},
     "Works also for sequences, and leaves other expressions unchanged.
     Copying the list v is always done when v is mutable.",
     EXAMPLE "deepSplice { (a,b,(c,d,(e,f))), g, h }",
     SeeAlso => "splice"
     }

document {
     Key => {splice,(splice, BasicList)},
     Headline => "remove subsequences",
     TT "splice v", " -- yields a new list v where any members of v that are sequences
     are replaced by their elements.",
     PARA{},
     "Works also for sequences, and leaves other expressions unchanged.
     Copying the list v is always done when v is mutable.
     Certain functions always splice their arguments or their argument
     lists for the sake of convenience.",
     EXAMPLE {
	  "splice ((a,b),c,(d,(e,f)))",
      	  "splice [(a,b),c,(d,(e,f))]",
	  },
     SeeAlso => "deepSplice"
     }

document {
     Key => MutableList,
     Headline => "the class of all mutable lists",
     PARA {"For an overview of lists and sequences, see ", TO "lists and sequences", "."},
     PARA{},
     "Normally the entries in a mutable list are not printed, to prevent
     infinite loops in the printing routines.  To print them out, use 
     ", TO "peek", ".",
     EXAMPLE {
	  "s = new MutableList from {a,b,c};",
      	  "s#2 = 1234;",
	  "s",
      	  "peek s",
	  },
     SeeAlso => {"BasicList"}
     }

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
