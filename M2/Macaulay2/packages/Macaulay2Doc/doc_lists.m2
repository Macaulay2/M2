--		Copyright 2018 by Daniel R. Grayson and Lily Silverstein

doc///
 Key
  "lists and sequences"
 Headline
  a detailed overview of lists and sequences in Macaulay2
 Description
  Text
   This page gives an overview of the use of lists of all types, including:
   
   {\bf basic lists} (of class @TO BasicList@),@BR{}@
   {\bf visible lists} (of class @TO VisibleList@),@BR{}@
   {\bf lists} (of class @TO List@),@BR{}@
   {\bf sequences} (of class @TO Sequence@),@BR{}@
   {\bf arrays} (of class @TO Array@),
   and@BR{}@
   {\bf mutable lists} (of class @TO MutableList@).
   
   The sections of the overview are, in order:
   
   {\bf Creating lists; kinds of lists.} @BR{}@
   {\bf Elements and indexing.} @BR{}@
   {\bf Nested lists.} @BR{}@
   {\bf Ranges and repetitions.} @BR{}@
   {\bf Manipulating lists and sequences.} @BR{}@
   {\bf Mapping over lists.} @BR{}@
   {\bf Conditional expressions; selecting elements matching a criterion.}
   
   Links to individual documentation pages for the functions 
   described in this article are collected in an alphabetized list 
   at the very end of the page.
   
   
   {\bf Creating lists; kinds of lists.}
   
   To create a {\bf list}, use curly braces around a comma-separated series of elements.
  Example
   L = {a,b,c,d,e}
  Text
   {\bf Sequences} are created and displayed using parentheses instead of braces.
  Example
   S = (a,b,c,d,e)
  Text
   Sequences are implemented in a more efficient way than lists, since a sequence 
   is created every time a function is called with more than one argument. 
   On the other hand, parentheses are also used for grouping algebraic 
   expressions. This complicates some tasks, such as creating a sequence of 
   length 1. 
  Example
   ()
   (1,2)
   (1)
  Text
   The functions @TO toList@ and @TO toSequence@, which convert between lists
   and sequences, may be useful here.
  Example
   toSequence {1}
  Text
   Lists and sequences can be used as vectors, provided their elements 
   are the sorts of things that can be added and mutliplied.
  Example
   10000*{3,4,5} + {1,2,3}
  Text
   An {\bf array} is another type of list, created and displayed using square brackets. Unlike lists and sequences, 
   arrays can't be used as vectors. Their main use is notational; for example, they appear in the construction of polynomial rings.
  Example
   v = [1,2,3]
   ZZ[a,b,c]
  Text
   Lists, sequences, and arrays are the three examples of what we call {\bf visible lists}, 
   which constitute the class @TO VisibleList@. Many functions are defined to act uniformly 
   on visible lists. There is a type of list more general than a visible list, which we 
   call a @TO BasicList@. 
   Basic lists are a secure choice for defining new datatypes, since the many functions 
   that act on visible lists do not act on basic lists. 
   Here we illustrate how to create a new type of basic list, in this case called {\em Container}.
  Example
   Container = new Type of BasicList
   t = new Container from {a,b}
  Text
   We can then declare a new method for the operator @TO "++"@ that will join two Containers:
  Example
   Container ++ Container := join;
   t ++ t
  Text
   Basic lists are normally {\em immutable}; that is, no element can
   be replaced, added, or removed. (Functions like @TO append@, and the many others 
   described in the {\bf Manipulating lists and sequences} section, will return a new list
   in {\tt Macaulay2}, as a general rule, rather than modify the existing list.)
   
   However, there is a certain type of basic list, called a {\bf mutable list} 
   (of class @TO MutableList@), whose elements can be changed.  
   This allows the possibility of creating circular structures that would cause a print 
   routine to go into an infinite loop. To avoid such infinite loops, the contents of mutable
   lists are not printed. Instead, use @TO peek@ to display the elements in a controlled way.
  Example
   A = new MutableList from {a,b,c}
   peek A
   A#2 = 1234;
   A
   peek A
  Text
   Because the contents of mutable lists are not printed, they can be used as containers for 
   big things one doesn't {\em want} to print. Although any mutable list can be used
   in this way, there is a a special type of mutable list called a @TO Bag@, 
   which is designed for this purpose. When printed, the bag displays only a little information
   about its contents.
  Example
   r = Bag {100!}; r
   q = Bag {1/100!}; q
   unbag q
  Text
   The hierarchy of types mentioned above is summarized here using @TO showStructure@:
  Example
   showStructure(List,Sequence,Array,Container,MutableList,Bag,BasicList)
  
  --
  Text

   {\bf Elements and indexing.}

   We retrieve the length of a list with the operator @TO "#"@ or with the function @TO length@.
  Example
   L = {926, 621, 429, 67, 594, 904, 264, 18, 35, 961};
   #L
   length L
  Text
   The expression $L\#n$ returns the $n$th element of $L$. The elements are numbered consecutively starting with 0. Alternatively, they are numbered consecutively ending with -1.
  Example
   L#0
   L#2
   L#-1
  Text
   The @TO "_"@ operator is similar to @TO "#"@, except that it can also take a list 
   or range of indices. However, it may only be used with visible lists.
  Example
   L_1 
   L_{3,6}
  Text
   The functions @TO first@ and @TO last@ retrieve the first and last elements of a list.
  Example
   first L
   last L
  Text
   Omitting an element of a list causes the symbol @TO null@ to be inserted in its place.
   When the value of an expression is {\tt null}, like when we ask for {\tt A#2} in the 
   next example, the output line doesn't appear at all. 
  Example
   A = {3,4,,5}
   peek A
   A#2
   
  --
  Text

   {\bf Ranges and repetitions.}

   The operator @TO ".."@ can be used to create sequences of numbers, sequences of subscripted variables, and so on.
  Example
   1 .. 5, x_1 .. x_5, a .. e
  Text
   The operator @TO (symbol :, ZZ, Thing)@ creates a sequence by replicating an element a given number of times.
  Example
   12:a
  Text
   Replicating something once is another way to create a sequence of length 1, which cannot be entered by simply typing parentheses.
  Example
   1:a
  Text
   Both @TO ".."@ and @TO ":"@ produce sequences, and may not behave as expected with respect to nesting. 
   For instance, to create the list {\tt \{3, 4, 5, 6, 9, 12, 12, 12\}}, the following command will {\em not} work: 
  Example
   A = {3 .. 6, 9, 3:12}
  Text
   Instead, we get a list of length 3, some of whose elements are sequences. 
   This is easily resolved with @TO splice@.
  Example
   A = splice {3..6, 9, 3:12}
  Text
   However, many operators and functions will automatically splice lists for you. 
   Two examples are the array of variables defining a polynomial ring, and the indices 
   passed to the @TO "_"@ operator.
  Example
   QQ[a..c,x_1..x_4]
   L_{1..3,-3..-1}
  
  --
  Text

   {\bf Nested lists.}
   
   When the elements of a list are themselves lists, we call it a {\em nested list}.
  Example
   A = {{a,b,c},{d,{e,f}}}
   #A
   #(first A)
  Text
   One level of nesting may be eliminated with @TO flatten@.
  Example
   flatten A
  Text
   The function @TO splice@ acts analogously on sequences, removing those pairs of parentheses that are one level inward.
  Example
   splice (a, (b, c), (d, (e, f, (g, h))) )
  Text
   To remove all layers of nesting at once, use @TO deepSplice@.
  Example
   deepSplice (a, (b, c), (d, (e, f, (g, h))) )
  Text
   A table is a list whose elements are lists all of the same length. The inner lists are regarded as rows when the table is displayed as a two-dimensional array with @TO MatrixExpression@.
  Example
   T = {{a,1},{b,2},{c,3}}
   isTable T
   MatrixExpression T
  Text
   The function @TO table@ can be used to create a table (doubly
   nested list) from two lists and a function of two arguments.  It applies
   the function consecutively to each element from the first list paired
   with each element from the second list, so the total number of evaluations
   of the function is the product of the lengths of the two lists.
  Example
   table({1,2,3},{7,8},(i,j) -> 1000*i+j)
  Text
   The function @TO pack@{\tt (L, n)} turns the list $L$ into a nested list, whose
   elements are lists containing the elements of $L$ taken $n$ at a time.
  Example
   pack(1..15, 4)
  Text
   On the other hand, @TO mingle@{\tt (L)} takes the nested list $L = \{L_1, L_2, \ldots, L_n\}$
   and combines the elements of the $L_i$ into a single list in the following way:
  Example
   mingle({{1,2,3,4}, {10,20,30,40}, {100,200,300,400}, {a, b}})

 
   
  --
  Text

   {\bf Manipulating lists and sequences.}

   Use @TO append@{\tt (L, x)} to create a copy of $L$ with the element $x$ added to the end. 
   Since lists are immutable in Macaulay2, $L$ itself is not changed, unless redefined.
  Example
   L = {926, 621, 429, 67, 594, 904, 264, 18, 35, 961};
   append(L, -10)
   L
   L = append(L, -10); L
  Text
   Use @TO prepend@{\tt (x, L)} to create a copy of $L$ with $x$ added to the beginning 
   of the list. Notice the order of arguments is the opposite of append!
  Example
   L = prepend(-20, L)
  Text
   Use @TO insert@{\tt (n, x, L)} to specify that the element $x$ should be added 
   to the list $L$ at position $n$.
  Example
   L = insert(5, -30, L)
  Text
   Use @TO switch@{\tt (m, n, L)} to switch the elements of $L$ in indices $m$ and $n$.
  Example
   L = switch(1, 2, L)
  Text
   The function @TO delete@ removes elements that have a particular {\em value}, NOT a particular {\em index}.
  Example
   L = delete(-10, L)
  Text
   To remove the element at index $n$, use the command @TO drop@{\tt (L, \{n,n\})}.
  Example
   L = drop(L, {1,1})
  Text
   You can also use drop to remove a specified number of elements from the beginning or end of $L$.
  Example
   L = drop(L, 2)
   L = drop(L, -2)
  Text
   On the other hand, use @TO take@ to specify the number of elements to {\em keep}, or a range of indices to keep:
  Example
   L = take(L, 6)
   L = take(L, {1,4})
  Text
   Use @TO between@{\tt (x, L)} to insert the element $x$ between every two elements of $L$.
  Example
   L = between(-5, L)
  Text
   Useful commands for reordering lists are @TO reverse@ (reverse the current order), @TO sort@ (put elements in ascending order), and @TO rsort@ (put elements in descending order). 
  Example
   L
   reverse L
   sort L
   rsort L
  Text
   Use @TO unique@ to remove duplicates from a list or sequence.
  Example
   unique L
  Text
   Use @TO join@ to concatenate two lists or sequences. The symbol @TO symbol|@ is also used for concatenation.
  Example
   join(a..f, 1..6)
   x_1..x_3 | y_1..y_4
   
  --
  Text

   {\bf Mapping over lists.}
   
   In programming, loops that operate on consecutive elements of a
   list are common, so we offer various ways to apply functions to
   each of the elements of a list, along with various ways to treat the
   returned values.

   The most basic operation is provided by @TO scan@, which applies
   a function consecutively to each element of a list, discarding the
   values returned.
  Example
   scan({a,b,c}, print)
  Text
   The keyword @TO "break"@ can be used to terminate the scan
   prematurely, and optionally to specify a return value for the
   expression.  Here we use it to locate the first even number in
   a list.
  Example
   scan({3,5,7,11,44,55,77}, i -> if even i then break i)
  Text
   The function @TO apply@ is similar to @TO scan@, but
   creates a list storing the values returned.
  Example
   apply({1,2,3,4}, i -> i^2)
  Text
   This operation is so common that we offer two shorthand notations for
   it, one with the function on the right and one with the function on
   the left.
  Example
   {1,2,3,4} / (i -> i^2)
   (i -> i^2) \ {1,2,3,4}
  Text
   The associativity of these operators during parsing is set up so the 
   following code works as one would wish.
  Example
   {1,2,3,4} / (i -> i^2) / (j -> 1000*j)
   (j -> 1000*j) \ (i -> i^2) \ {1,2,3,4}
   (j -> 1000*j) @@ (i -> i^2) \ {1,2,3,4}
  Text
   The function @TO apply@ can also be used with two lists of the same
   length, in which case it will apply the function consecutively to
   corresponding elements of the two lists.
  Example
   apply({1,2,3}, {7,8,9}, (i,j) -> 1000*i+j)
  Text
   The function @TO applyTable@ can be used to apply a function to 
   each element of a table.
  Example
   applyTable( {{1,2,3},{4,5}}, i -> i^2)
  Text
   The functions @TO fold@ and @TO accumulate@ provide various
   ways to iteratively apply a function of two arguments to the elements of a list.  One
   of the arguments is the next element from the list, and the other argument
   is the value returned by the previous application of the function.  As an
   example, suppose we want to convert the list {\tt \{7,3,5,4,2\}} of digits
   into the corresponding number {\tt 73542}.  The formula
   {\tt (((7*10+3)*10+5)*10+4)+2} is a fast way to do it that doesn't
   involve computing high powers of 10 separately.  We can do this with
   @TO fold@ and the following code.
  Example
   fold((i,j) -> i*10+j, {7,3,5,4,2})
  Text
   Using @TO accumulate@ returns all the intermediate values of this iterative 
   calculation along with the final result.
  Example
   accumulate((i,j) -> i*10+j, {7,3,5,4,2})
   
  --
  Text
  
   {\bf Conditional expressions; selecting elements matching a criterion.}

   Use @TO select@ to select those elements from a list
   that satisfy some condition.  
  Example
   select({12, 3, -10, 42, 7, 6, 53}, even)
   select({12, 3, -10, 42, 7, 6, 53}, i -> i<0 or i>40)
  Text
   An optional first argument of an integer $n$ specifies to select no more
   than $n$ matching elements.
  Example
   select(2, {12, 3, -10, 42, 7, 6, 53}, even)
  Text 
   Use @TO positions@ to select the {\em indices} of elements satisfying
   the condition.
  Example
   positions({12, 3, -10, 42, 7, 6, 53}, i -> i<0 or i>40)
  Text
   The singular @TO position@ returns only the first (or last, by specifying
   {\tt Reverse => true}) matching index.
  Example
   position({12, 3, -10, 42, 7, 6, 53}, i -> i<0 or i>40)  
   position({12, 3, -10, 42, 7, 6, 53}, i -> i<0 or i>40, Reverse => true)
  Text
   Use @TO number@ to count how many elements satisfy the condition.
  Example
   number({12, 3, -10, 42, 7, 6, 53}, i -> i<0 or i>40)
  Text
   Use @TO max@ and @TO min@ to find the maximum or minimum of the list. 
   The functions @TO maxPosition@ and @TO minPosition@ give the index of the
   maximum/minimum. Note that max and min work with many {\tt Macaulay2}
   types besides numbers, for instance elements of a polynomial ring with 
   respect to a monomial order.   
  Example
   R = QQ[x,y,z, MonomialOrder => Lex];
   max {x^2*y*z, x*y^3*z^2, x^3, y^3, z}
   maxPosition {x^2*y*z, x*y^3*z^2, x^3, y^3, z}
   min {x^2*y*z, x*y^3*z^2, x^3, y^3, z}
   minPosition {x^2*y*z, x*y^3*z^2, x^3, y^3, z}
  Text
   We may use @TO any@ to tell whether there is at least one element of 
   a list satisfying a condition, and @TO all@ to tell whether all 
   elements satisfy it.
  Example
   any({3, 6, 7, 8}, even)
   all({3, 6, 7, 8}, even)
 SeeAlso
  (symbol #, BasicList)
  (symbol #, BasicList, ZZ)
  (symbol #?, BasicList, ZZ)
  (symbol _, VisibleList, ZZ)
  (symbol _, VisibleList, List)
  (symbol|,List,List)
  (all,BasicList,Function)
  (any,BasicList,Function)
  append
  (apply,BasicList,Function)
  between
  deepSplice
  delete
  drop
  first
  flatten
  fold
  insert
  join
  last
  mingle
  number
  pack
  position
  positions
  prepend
  reverse
  rsort
  same
  (scan,BasicList,Function)
  select
  sort
  splice
  switch
  table
  take
  uniform
  unique
  sequence
  toList
  toSequence
  unsequence
///

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
     SeeAlso => {splice, (symbol..,ZZ,ZZ), "lists and sequences"}
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

document {
     Key => sequence,
     Headline => "make a sequence",
     Usage => "sequence v",
     Inputs => { "v" => Thing },
     Outputs => { Sequence => {TT "v", " if ", TT "v", " is a sequence, otherwise a sequence of length 1 containing ", TT "v"}},
     PARA { "Such a function is needed occasionally to restore uniformity, because a nonempty parenthesized expression with no commas is not parsed as a sequence." },
     EXAMPLE {
	  "sequence()",
	  "sequence(4)",
      	  "sequence(4,5)",
	  "identity()",
	  "identity(4)",
      	  "identity(4,5)",
	  },
     SeeAlso => { unsequence }
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
