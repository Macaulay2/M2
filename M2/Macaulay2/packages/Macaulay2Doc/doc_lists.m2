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
  delete
  drop
  first
  flatten
  fold
  insert
  join
  last
  mingle
  pack
  position
  positions
  prepend
  reverse
  rsort
  (scan,BasicList,Function)
  select
  sort
  switch
  table
  take
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

doc///
 Key
  accumulate
  (accumulate, Function, Thing, VisibleList)
  (accumulate, Function, VisibleList)
  (accumulate, VisibleList, Thing, Function)
  (accumulate, VisibleList, Function)
 Headline
  apply a binary operator repeatedly
 Usage
  accumulate(f, x, L)
  accumulate(f, L)
  accumulate(L, x, f)
  accumulate(L, f)
 Inputs
  f:Function
  x:Thing
  L:VisibleList
 Outputs
  M:List
 Description
  Text
   Suppose {\tt L=\{x0, x1, ..., xn\}}. Then for any binary operator {\tt f}, 
   {\tt accumulate(f, L)} returns the list {\tt \{f(x0, x1), f(f(x0, x1), x2), ...\} }. 
   In other words, the binary operator is applied
   to the first two elements of {\tt L}, then to that result along with the next unused element of
   {\tt L}, and so forth.
  Example
   accumulate(plus, {0,1,2,3,4,5})
   accumulate(concatenate, {a,b,c,d,e})
   accumulate((i,j) -> i|j|i, {"a","b","c","d","e"})
  Text
   If {\tt accumulate(f, x, L)} is called, the element {\tt x} is used as the first argument of the
   binary function {\tt f}. In other words, {\tt accumulate(f, \{x0, x1, \ldots, xn\})} is 
   equivalent to {\tt accumulate(f, x0, \{x1, \ldots, xn\})}.
  Example
   accumulate(plus, 0, {1,2,3,4,5})
   accumulate((x, y) -> x^y, 2, {3,2,1,2})
  Text
   The function {\tt accumulate(\{x_0, x_1, \ldots, x_n\}, f)} returns the
   list {\tt \{..., f(x_{n-2}, f(x_{n-1}, x_n)), f(x_{n-1}, x_n) \} }. That is, {\tt f} is applied
   to the last two elements of the list, and the result placed at the end of the output. Then 
   the accumulation proceeds backwards through the list. The optional argument {\tt x} in
   {\tt accumulate(L, x, f)} is used as the second argument in the first evaluation of
   {\tt f}. So {\tt accumulate(\{x_0, x_1, \ldots, x_{n-1}\}, x_n, f)} is equivalent
   to {\tt accumulate(\{x_0, x_1, \ldots, x_n\}, f)}.
  Example
   accumulate({a,b,c,d,e}, concatenate)
   accumulate({a,b,c,d}, e, concatenate)  
   accumulate({2,3,2,1}, 2, (x, y) -> x^y)
  Text
   The difference between {\tt fold} and @TO accumulate@ is that {\tt fold} returns the
   final result of all the nested evaluations of {\tt f}, while {\tt accumulate} lists 
   all the intermediate values as well.
  Example
   fold({2,3,2,1}, 2, (x,y) -> x^y)
 SeeAlso
  apply
  fold
  "lists and sequences"
///

doc///
 Key
  commonest
  (commonest, VisibleList)
  (commonest, Set)
  (commonest, Tally)
 Headline
  the most common elements of a list or tally
 Usage
  commonest A
 Inputs
  A:VisibleList
 Outputs
  L:List
   a list of the elements of {\tt A} with the most repetitions
 Description
  Text
   If a single element is the most common, a list of length one is the output.
  Example
   commonest {a,a,a,a,b,b,b,c,c,d,e}
  Text
   In the case of a tie, all commonest elements are returned.
  Example
   A = {a,a,a,a,b,b,b,b,c,c,c,c,d,e}; commonest A
  Text
   {\tt commonest} works on @TO Tally@s and @TO Set@s as well.
  Example
   T = tally A
   commonest T
   S = set A
   commonest S
  Text
   (Since every element of a set is unique, it is unclear why one would need {\tt commonest(Set)}.)
 SeeAlso
  number
  same
  set
  tally
  unique
  "lists and sequences"
///

doc///
 Key
  delete
 Headline
  delete some elements of a list
 Usage
  delete(x, A)
 Inputs
  A:
   list or sequence
  x:
   thing
 Outputs
  A2:
    a new list from {\tt A} with every occurrence of {\tt x} removed
 Description
  Example
   delete(c, {a,b,c,d,e,a,b,c,d,e})
  Text
   Equality is determined with @TO"==="@, which is quick, but not always
   intuitive. For instance, in the next example, the first item in the list is
   {\bf not} removed, because it is an element of {\tt QQ} and will not match
   an element of {\tt ZZ}.
  Example
   delete(1, {2/2, 3/2, 4/2})
  Text
   To delete items from a list by index, rather than value, see @TO drop@.
 SeeAlso
  drop
  positions
  select
  "lists and sequences"
///

doc///
 Key
  demark
 Headline
  insert a string between elements of a list of strings
 Usage
  demark(d, L)
 Inputs
  d: String
  L: List
   of strings
 Outputs
  s: String
   the string obtained by concatenating the elements of {\tt L}, with copies
   of the string {\tt d} inserted between each pair
 Description
  Example
   demark("+", a..f)
   demark(" and ", 6:"more")
  Text
   To achieve a similar insertion while keeping the output as a list, see @TO mingle@.
  Example
   mingle(6: "more", 5: "and")
 SeeAlso
  insert
  join
  mingle
  "lists and sequences"
///

doc ///
 Key
  drop
  (drop, BasicList, ZZ)
  (drop, BasicList, List)
 Headline
  Drop some elements from a list or sequence.
 Usage
  drop(L, i)
  drop(L, {j,k})
 Inputs
  L: BasicList
  i: ZZ
  j: ZZ
  k: ZZ
 Outputs
  L2: BasicList
   the list or sequence obtained by dropping the first {\tt i} elements of {\tt L},
   (if {\tt i} positive), or the last {\tt i} elements of {\tt L} (if {\tt i} negative), or, if given the
   pair {\tt j,k}, the list or sequence obtained by dropping the elements of {\tt L} with indices {\tt j} through {\tt k}
 Description
  Example
   drop({a,b,c,d,e,f,g}, 3)
   drop({a,b,c,d,e,f,g}, -3)
   drop({a,b,c,d,e,f,g}, {1,3})
   drop({a,b,c,d,e,f,g}, {2,2})    
  Text
   The pair {\tt \{j,k\}} must be given with both entries non-negative, and $j\le k$. Otherwise the original list is returned.
  Example
   drop({a,b,c,d,e,f,g}, {3,1})
   drop({a,b,c,d,e,f,g}, {4,-1})
 SeeAlso
  take
  delete
  position
  positions
  select
  "lists and sequences"
///

doc///
 Key
  first
 Headline
  first element of a list
 Usage
  first L
 Inputs
  L:
   a list or sequence
 Outputs
  f:
   the first element of {\tt L}
 Description
  Example
   first {a,b,c,d,e}
   first gens(QQ[x,y,z])
 SeeAlso
  last
  take
  select
  position
  "lists and sequences"
///

doc///
 Key
  fold
  (fold, Function, Thing, VisibleList)
  (fold, Function, VisibleList)
  (fold, VisibleList, Thing, Function)
  (fold, VisibleList, Function)
 Headline
  apply a binary operator repeatedly
 Usage
  fold(f, x, L)
  fold(f, L)
  fold(L, x, f)
  fold(L, f)
 Inputs
  f:Function
  x:Thing
  L:VisibleList
 Outputs
  M:List
 Description
  Text
   Suppose {\tt L=\{x0, x1, ..., xn\}}. Then for any binary operator {\tt f}, 
   {\tt fold(f, L)} computes {\tt f(...f(f(x0, x1), x2), ...)}. 
   In other words, the binary operator is applied
   to the first two elements of {\tt L}, then to that result along with the next unused element of
   {\tt L}, and so forth.
  Example
   fold(plus, {0,1,2,3,4,5})
   fold(identity, {a,b,c,d,e})
   fold((i,j) -> i|j|i, {"a","b","c","d","e"})
  Text
   If {\tt fold(f, x, L)} is called, the element {\tt x} is used as the first argument of the
   binary function {\tt f}. In other words, {\tt fold(f, \{x0, x1, \ldots, xn\})} is 
   equivalent to {\tt fold(f, x0, \{x1, \ldots, xn\})}.
  Example
   fold(plus, 0, {1,2,3,4,5})
   fold((x, y) -> x^y, 2, {3,2,1,2})
  Text
   The function {\tt fold(\{x_0, x_1, \ldots, x_n\}, f)} returns 
   {\tt f...f(f(x_{n-2}, f(x_{n-1}, x_n)))}. That is, {\tt f} is applied
   to the last two elements of the list first, then the repeated calls to
   {\tt f} proceed backwards through the list. The optional argument {\tt x} in
   {\tt fold(L, x, f)} is used as the second argument in the first evaluation of
   {\tt f}. So {\tt fold(\{x_0, x_1, \ldots, x_{n-1}\}, x_n, f)} is equivalent
   to {\tt fold(\{x_0, x_1, \ldots, x_n\}, f)}.
  Example
   fold({a,b,c,d,e}, identity)
   fold({a,b,c,d}, e, identity)  
   fold({2,3,2,1}, 2, (x, y) -> x^y)
  Text
   The difference between @TO fold@ and {\tt accumulate} is that {\tt fold} returns the
   final result of all the nested evaluations of {\tt f}, while {\tt accumulate} lists 
   all the intermediate values as well.
  Example
   accumulate({2,3,2,1}, 2, (x, y) -> x^y)
 SeeAlso
  apply
  accumulate
  "lists and sequences"
///

doc///
 Key
  insert
  (insert,ZZ,Thing,VisibleList)
 Headline
  copy a list, inserting an element
 Usage
  insert(i, x, L)
 Inputs
  i: ZZ
  x: Thing
  L: VisibleList
 Outputs
  L2: VisibleList
   a copy of {\tt L} in which {\tt x} has been inserted into position {\tt i}
 Description
  Example
   L = 0..10
   insert(4, "hi", L)
   insert(0, "hi", L)
   insert(11, "hi", L)
   insert(-1, "hi", L)
   apply({-1,-3,-5}, i -> L = insert(i, "hi", L)); L
 SeeAlso
  delete
  mingle
  switch  
  "lists and sequences"
///

doc///
 Key
  isSorted
  (isSorted, VisibleList)
 Headline
  whether a list is sorted
 Usage
  isSorted L
 Inputs
  L: VisibleList
 Outputs
   : Boolean
    whether the elements of {\tt L} are in increasing order
 Description
  Example
   isSorted {1,2,2,3}
   isSorted {1,2,3,2}
   R = ZZ/2[x,y,z, MonomialOrder => Lex]; 
   isSorted (z^3, y^2, x)
   R = ZZ/2[x,y,z, MonomialOrder => GLex]; 
   isSorted (z^3, y^2, x)   
 SeeAlso
  sort
  "?"
  "lists and sequences"
///

doc///
 Key
  join
 Headline
  join lists and sequences
 Usage
  join(A, B, ...)
 Inputs
  A: BasicList
  B: BasicList
 Outputs
  Z: BasicList
 Description
  Text
   {\tt join(A, B, ...)} joins the elements of the lists or sequences
   {\tt A, B, ...} into a single list or sequence. The inputs may belong
   to different classes; the class of the result will match the class of
   the first argument passed to {\tt join}.
  Example
   join( {1,2,3}, (4,5,6), (7,8,9) )
   join( (1,2,3), {4,5,6}, {7}, (8,9,10) )
  Text
   The operator @TO"|"@ is a convenient shorthand for joining two
   inputs of the same class. 
  Example
   {1,2,3} | {4,5,6}
   (1,2,3) | (4,5,6)
 SeeAlso
  concatenate
  demark
  flatten
  mingle
  "List | List"
  "lists and sequences"
///

doc///
 Key
  last
 Headline
  last element of a list
 Usage
  last L
 Inputs
  L:
   a list or sequence
 Outputs
  l:
   the last element of {\tt L}
 Description
  Example
   last {a,b,c,d,e}
   last gens(QQ[x,y,z])
 SeeAlso
  first
  take
  select
  position
  "lists and sequences"
///

doc///
 Key
  max
  (max, VisibleList)
 Headline
  yields the maximum element in a list or sequence
 Usage
  max X
 Inputs
  X: VisibleList
 Outputs
  m: Thing
 Description
  Example
   X = for i from 1 to 10 list random(100)
   max X
  Text
   If {\tt L} contains elements in a polynomial ring, the @TO MonomialOrder@
   of the ring is used for comparisons.
  Example
   R1 = QQ[x, y, z, MonomialOrder => Lex];
   max {x*y^2, x*y^2 + z^2, y^4, y*z^5}
   R2 = QQ[x, y, z, MonomialOrder => GRevLex];
   max (x*y^2, x*y^2 + z^2, y^4, y*z^5)
  Text
   More generally, the order of the elements is determined using the @TO "?"@ operator.
 
   If {\tt X} is a list of lists, {\tt max} acts on the outermost level.
  Example    
   max {{3, 1, 2}, {2, 9, 6}, {3, 7, 5}}
   max flatten {{3, 1, 2}, {2, 9, 6}, {3, 7, 5}}
 SeeAlso 
  maxPosition
  min
  sort
  "?"
///
   
doc///
 Key 
  maxPosition
  (maxPosition, BasicList)
 Headline
  position of the largest element
 Usage
  maxPosition L
 Inputs
  L:BasicList
 Outputs
  i:ZZ
   the index of the largest element in the list {\tt L}
 Description
  Text
   If the largest element occurs more than once, the index of its first occurrence is used.
  Example
   maxPosition {1, 6, 4, 2, 6}
  Text
   If {\tt L} contains elements in a polynomial ring, the @TO MonomialOrder@
   of the ring is used for comparisons.
  Example
   R1 = QQ[x, y, z, MonomialOrder => Lex];
   maxPosition {x*y^2, x*y^2 + z^2, y^4, y*z^5}
   R2 = QQ[x, y, z, MonomialOrder => GRevLex];
   maxPosition (x*y^2, x*y^2 + z^2, y^4, y*z^5)
  Text
   More generally, the order of the elements is determined using the @TO "?"@ operator.
 SeeAlso 
  minPosition
  max
  min
  sort
  position
  positions
  "?"
///

doc///
 Key
  min
  (min, VisibleList)
 Headline
  yields the minimum element in a list or sequence
 Usage
  min X
 Inputs
  X: VisibleList
 Outputs
  m: Thing
 Description
  Example
   X = for i from 1 to 10 list random(100)
   min X
  Text
   If {\tt L} contains elements in a polynomial ring, the @TO MonomialOrder@
   of the ring is used for comparisons.
  Example
   R1 = QQ[x, y, z, MonomialOrder => Lex];
   min {x*y^2, x*y^2 + z^2, y^4, y*z^5}
   R2 = QQ[x, y, z, MonomialOrder => GRevLex];
   min (x*y^2, x*y^2 + z^2, y^4, y*z^5)
  Text
   More generally, the order of the elements is determined using the @TO "?"@ operator.
 
   If {\tt X} is a list of lists, {\tt min} acts on the outermost level.
  Example    
   min {{3, 1, 2}, {2, 9, 6}, {3, 7, 5}}
   min flatten {{3, 1, 2}, {2, 9, 6}, {3, 7, 5}}
 SeeAlso 
  minPosition
  max
  sort
  "?"
///

doc///
 Key
  mingle
  (mingle, BasicList)
 Headline
  mingle elements of several lists
 Usage
  mingle(L)
 Inputs
  L:BasicList
   a list of lists {\tt L=\{L1, L2, ..., Ln\}}
 Outputs
  M:List
   a new list mingling the elements of all lists in {\tt L}
 Description
  Text
   The output list {\tt M} takes the first element of each {\tt Li, i=1,...,n}, followed by
   the second element of {\tt Li, i=1,...,n}, and so forth.  
  Example
   mingle {{a1, a2, a3}, {b1, b2, b3}, {c1, c2, c3}}
  Text
   The lists can have different lengths. After a list is exhausted, it
   will be silently ignored.
  Example
   mingle {{a1, a2, a3, a4}, {b1, b2}, {c1}}
  Text
   To transpose a nested list (thinking of it as a matrix), try
   using {\tt mingle} with @TO pack@.
  Example
   pack(3, mingle ((a1, a2, a3), (b1, b2, b3), (c1, c2, c3)))
  Text
   Notice from the previous example that {\tt mingle} accepts sequences and
   other types of @TO BasicList@s as input, but the output will always be a 
   @TO List@. 
  Text
   Further examples:
  Example
   concatenate mingle( {"a","b","c"} , {",",","} )
   netList pack(3, mingle( (0..5), apply(6, i -> i^2), apply(6, i -> i^3)))
 SeeAlso
  insert
  join
  pack
  sort
  apply
  "lists and sequences"
///

doc///
 Key 
  minPosition
  (minPosition, BasicList)
 Headline
  position of the smallest element
 Usage
  minPosition L
 Inputs
  L:BasicList
 Outputs
  i:ZZ
   the index of the smallest element in the list {\tt L}
 Description
  Text
   If the smallest element occurs more than once, the index of its first occurrence is used.
  Example
   minPosition {2, 1, 6, 4, 1}
  Text
   If {\tt L} contains elements in a polynomial ring, the @TO MonomialOrder@
   of the ring is used for comparisons.
  Example
   R1 = QQ[x, y, z, MonomialOrder => Lex];
   minPosition {x*y^2, x*y^2 + z^2, y^4, y*z^5}
   R2 = QQ[x, y, z, MonomialOrder => GRevLex];
   minPosition (x*y^2, x*y^2 + z^2, y^4, y*z^5)
  Text
   More generally, the order of the elements is determined using the @TO "?"@ operator.
 SeeAlso 
  maxPosition
  max
  min
  sort
  position
  positions
  "?"
///

doc///
 Key
  number
 Headline
  count how many elements of a list satisfy a condition
 Usage
  number(A, f)
 Inputs
  A:
   a list or sequence
  f:
   a boolean function
 Outputs
  c:
   an integer, the number of elements of {\tt A} that satisfy {\tt f}
 Description
  Example
   number(0..100, isPrime)
   number(0..100, odd)
   number(0..100, i -> i==17)
  Text
   To find the first or last index of an element satisfying the condition, see @TO position@. 
   For all indices that match the condition, see @TO positions@. To return the 
   elements, rather than their indices, see @TO select@. 
  Example
   position((10,20,43,105,6), odd)  
   positions((10,20,43,105,6), odd)
   select((10,20,43,105,6), odd)
 SeeAlso
  all
  any
  commonest
  position
  positions
  same
  select
  tally
  "lists and sequences"
///


doc///
 Key
  pack
  (pack, BasicList, ZZ)
  (pack, ZZ, BasicList)
 Headline
  pack elements of a list into several shorter lists
 Usage
  pack(A, n)
  pack(n, A)
 Inputs
  A: BasicList
  n: ZZ
   how many elements of {\tt A} to put in each new list
 Outputs
  L: List
   a list of lists, with the elements of {\tt A} taken {\tt n} at a time.
 Description
  Text
   The commands {\tt pack(A, n)} and {\tt pack(n, A)} produce identical results.
  Example
   pack(a..l, 3)
   pack(3, a..l)
  Text
   If {\tt n} doesn't divide the length of {\tt A}, the last list will have fewer
   than {\tt n} elements.
  Example
   pack(a..m, 3)
  Text
   {\tt pack} and @TO mingle@ can be used together to take a transpose of lists
  Example
   pack(2, mingle(a..m, 0..12))
 SeeAlso
  mingle
  sort
  take
  "lists and sequences"
///
 

doc///
 Key
  position
  (position, VisibleList, Function)
  (position, VisibleList, VisibleList, Function)
  [position, Reverse]
 Headline
  the first element of a list satisfying a condition
 Usage
  position(A, f)
  position(A, B, f)
  position(A, f, Reverse => true)
 Inputs
  A: VisibleList
  B: VisibleList
  f: Function
 Outputs
  p: ZZ
   the first index to satisfy the boolean function {\tt f}
 Description
  Text
   {\tt position(A, f)} returns the smallest index {\tt i} such that {\tt f(A#i)} 
   is true. If no element satisfies the condition, @TO null@ is returned.
  Example
   position((10,20,43,105,6,93), odd)
   position((10,20,43,105,6,93), i -> i<0)
  Text
   Use {\tt position(A, B, f)} to return the smallest index {\tt i} such that {\tt f(A#i, B#i)}
   is true.
  Example
   position((10,20,43,105,6,93),(18,82,12,7,35,92), (a,b) -> a>b)
  Text
   The {\tt Reverse} option will return the largest index instead.
  Example
   position((10,20,43,105,6,93), odd, Reverse => true)  
   position((10,20,43,105,6,93),(18,82,12,7,35,92), (a,b) -> a>b, Reverse => true)
  Text
   To find all indices of elements satisfying the condition, see @TO positions@. To return the 
   elements, rather than their indices, see @TO select@. The function @TO number@ counts the
   number of elements satisfying the condition.
  Example
   positions((10,20,43,105,6,93), odd)
   select((10,20,43,105,6,93), odd)
   number((10,20,43,105,6,93), odd)
 SeeAlso
  minPosition
  maxPosition
  number
  positions
  select
  take
  "lists and sequences"
///

doc///
 Key
  positions
  (positions, VisibleList, Function)
 Headline
  which elements of a list satisfy a condition
 Usage
  positions(A, f)
 Inputs
  A: VisibleList
  f: Function
 Outputs
  p: List
   the list of indices {\tt i} such that {\tt f(A#i)} is true
 Description
  Text
   The indices are listed in ascending order. If no element satisfies the condition, an empty list is returned.
  Example
   positions((10,20,43,105,6,93), odd)
   positions((10,20,43,105,6,93), i -> i<0)
   positions(100..110, isPrime)
  Text
   To find the first or last index of an element satisfying the condition, see @TO position@. To return the 
   elements, rather than their indices, see @TO select@. The function @TO number@ counts the
   number of elements satisfying the condition.
  Example
   position((10,20,43,105,6), odd)  
   position((10,20,43,105,6), odd, Reverse => true)
   select((10,20,43,105,6), odd)
   number((10,20,43,105,6), odd)
 SeeAlso
  minPosition
  maxPosition
  number
  position
  select
  take
  "lists and sequences"
///

doc///
 Key
  reverse
  (reverse, BasicList)
 Headline
  reverse a list or sequence
 Usage
  reverse(L)
 Inputs
  L:BasicList
 Outputs
  R:BasicList
   a BasicList containing the elements of {\tt L} in reverse order
 Description
  Text
   The output list will be the same type as the input.
  Example
   reverse {5, 7, 2, 8}
   reverse (5, 7, 2, 8)
 SeeAlso
  sort
///

doc///
 Key
  same
 Headline
  whether everything in a list is the same
 Usage
  same L
 Inputs
  L:
   a list
 Outputs
  b:
   a Boolean
 Description
  Example
   same {1, 1, 1, 1}
   same {1, 2, 1, 1}
  Text
   The comparison is done with "===", which is quick, but not always intuitive. Here is a 
   simple example of what can go wrong:
  Example
   R = QQ[x,y,z]; 
   L = {gcd{x,y}, x/x, 1}
   same L
  Text
   We can see the problem by asking {\tt Macaulay2} to display the class of each element of {\tt L}.
  Example
   apply(L, class)
  Text
   The first {\tt 1} is an element of the ring {\tt R}, the second {\tt 1} is an
   element of the fraction field of {\tt R}, and the third {\tt 1} is an integer. Thus
   {\tt Macaulay2} thinks of these three elements as being pairwise unequal.
 SeeAlso
  commonest
  number
  set
  unique
  "lists and sequences"
///

doc///
 Key
  scan
  (scan, BasicList, Function)
  (scan, ZZ, Function)
 Headline
  apply a function to each element in a list or sequence
 Usage
  scan(L, f)
  scan(n, f)
 Inputs
  L: BasicList
  n: ZZ
  f: Function
 Outputs
  : null
 Description
  Text
   {\tt scan(L, f)} applies the function {\tt f} to each element
   of the list {\tt L}. The function values are discarded.
  Example
   scan({a, 4, "George", 2^100}, print)
  Text
   {\tt scan(n, f)} applies the function {\tt f} to each element 
   of the list 0, 1, ..., n-1
  Example
   scan(4, print)
   v = {a,b,c}; scan(#v, i -> print(i,v#i))
  Text
   The keyword @TO break@ can be used to terminate the scan prematurely, 
   and optionally to specify a return value for the expression. Here we
   use it to locate the first even number in a list.
  Example
   scan({3,5,7,11,44,55,77}, i -> if even i then break i)
 SeeAlso
  apply
  accumulate
  fold
  "lists and sequences"
///
   
doc///
 Key
  subsets
  (subsets, ZZ)
  (subsets, ZZ, ZZ)
  (subsets, List)
  (subsets, List, ZZ)
  (subsets, Sequence, ZZ)
  (subsets, Set)
  (subsets, Set, ZZ)
 Headline
  produce the subsets of a set or list
 Usage
  subsets(A)
  subsets(A, n)
 Inputs
  A: List
   , sequence, set or integer
  n: ZZ
   optional input to specify subsets of a particular size
 Outputs
  L: List
   of subsets (of size {\tt n} if given)
 Description
  Text
   If {\tt A} is an integer, {\tt subsets(A)} lists the subsets of {\tt \{0, 1, ..., A-1\}}.
  Example
   subsets(3)
   subsets(5, 3) 
  Text
   {\tt A} can be a list, sequence, or set. The elements need not be of the same type.
  Example
   subsets({"apple", "banana", {1,2,3}, 7.1}, 3)
  Text
   If a list contains repetitions, so will the subsets of that list. 
   Since a @TO Set@ has no repetitions, neither do its subsets. Also, 
   the subsets of a set will again be sets (while the subsets of a list are lists).
  Example
   subsets({"apple", "apple", "banana"})
   subsets(set{"apple", "apple", "banana"})
  Text
   The subsets of a Sequence are lists, not sequences. Also, a subset size {\bf must} be 
   specified when calling {\tt subsets} on a sequence.
 SeeAlso
  partitions
  set
  "lists and sequences"
///

doc///
 Key
  switch
  (switch,ZZ,ZZ,VisibleList)
 Headline
  copy a list, switching two elements
 Usage
  switch(i, j, L)
 Inputs
  i: ZZ
  j: ZZ
  L: VisibleList
 Outputs
  L2:
   a copy of the list {\tt L}, with the elements in positions {\tt i} and {\tt j} interchanged. 
 Description
  Text
   A negative value of {\tt i} or {\tt j} is taken relative to the end of the list.
  Example
   L = 0..10;
   switch(3, 9, L)
   switch(0, -1, L)
   switch(-1, -2, L)
 SeeAlso
  insert
  reverse
  "lists and sequences"
///
   
doc ///
 Key
  take
  (take, BasicList, ZZ)
  (take, BasicList, List)
 Headline
  Take some elements from a list or sequence.
 Usage
  take(L, i)
  take(L, {j,k})
 Inputs
  L: BasicList
  i: ZZ
  j: ZZ
  k: ZZ
 Outputs
  L2: BasicList
   the list or sequence containing the first {\tt i} elements of {\tt L},
   (if {\tt i} positive), or the last {\tt i} elements of {\tt L} (if {\tt i} negative), or, if given the
   pair {\tt j,k}, the list or sequence containing the elements of {\tt L} with indices {\tt j} through {\tt k}
 Description
  Example
   take({a,b,c,d,e,f,g}, 3)
   take({a,b,c,d,e,f,g}, -3)
   take({a,b,c,d,e,f,g}, {1,3})
   take({a,b,c,d,e,f,g}, {2,2})    
  Text
   The pair {\tt \{j,k\}} must be given with both entries non-negative, and $j\le k$. Otherwise an empty list is returned.
  Example
   take({a,b,c,d,e,f,g}, {3,1})
   take({a,b,c,d,e,f,g}, {4,-1})
 SeeAlso
  drop
  select
  position
  positions
  "lists and sequences"
///

doc///
 Key
  toList
  (toList, BasicList)
  (toList, Set)  
 Headline
  create a list
 Usage
  toList A
 Inputs
  A:
   a @TO Set@, or a @TO BasicList@ such as a @TO Sequence@
 Outputs
  L:List
   a list whose elements are the elements of {\tt A}
 Description
  Example
   A = set(3,7,9,6)
   toList A
  Text
   The command {\tt toList 1..9} will throw an error, because {\tt toList}
   comes before @TO".."@ in Macaulay2's order of operations. To create a list
   from a range, use {\tt toList (1..9)} instead.
  Example
   toList (1..9)
  Text
   Converting between list types may change the order of the elements in
   unexpected ways.
  Example
   toList set {4,5,13}
 SeeAlso
  BasicList
  List
  toSequence
  "lists and sequences"
///

doc///
 Key
  unique
  (unique, List)
  (unique, Sequence)
 Headline
  eliminate duplicates from a list
 Usage
  unique(L)
 Inputs
  L:List
   or sequence
 Outputs
  M:List
   the elements of {\tt L} without duplicates
 Description
  Text
   The output list maintains the order of elements in {\tt L}.
  Example
   unique {3,2,1,3,2,4,a,3,2,3,-2,1,2,4}
  Text
   Another way to list the unique elements of {\tt L} is by creating a
   set from {\tt L} and then listing its elements. This may be slightly
   faster than {\tt unique}, but forgets the ordering of {\tt L}.
  Example
   toList set {3,2,1,3,2,4,a,3,2,3,-2,1,2,4}
  Text
   To count occurrences of each element, use @TO tally@. To create
   a sorted list, see @TO sort@. For an overview of lists and sequences,
   see @TO"lists and sequences"@.
 SeeAlso 
  same
  sort
  set
  tally
  "lists and sequences"
///

TEST ///
    --accumulate
     assert( accumulate(toList,a,{b,c,d}) == {{a, b}, {{a, b}, c}, {{{a, b}, c}, d}} )
     assert( accumulate({a,b,c},d,toList) == {{a, {b, {c, d}}}, {b, {c, d}}, {c, d}} )
     assert( accumulate(toList,{a,b,c,d}) == {{a, b}, {{a, b}, c}, {{{a, b}, c}, d}} )
     assert( accumulate({a,b,c,d},toList) == {{a, {b, {c, d}}}, {b, {c, d}}, {c, d}} )
    --fold
     assert( fold(toList, a, {b,c,d}) === {{{a, b}, c}, d} )
     assert( fold({a,b,c}, d, toList) === {a, {b, {c, d}}} )
     assert( fold(toList, {a,b,c,d}) === {{{a, b}, c}, d} )
     assert( fold({a,b,c,d}, toList) === {a, {b, {c, d}}} )
    --max
     assert(max{4,5,6} === 6)
     assert(max(4,5,6) === 6)
    --min
     assert(min{4,5,6} === 4)
     assert(min(4,5,6) === 4)    
    --position
     assert( 3 === position({a,b,c,d,e,f},i->i===d ) )
    --subsets
     assert( subsets(4,2) === {{0,1},{0,2},{1,2},{0,3},{1,3},{2,3}} )
     assert( subsets({a,b,c,d},2) === {{a,b},{a,c},{b,c},{a,d},{b,d},{c,d}} )
     assert( 
      set subsets(set {a,b,c,d},2) === 
      set apply({{a,b},{a,c},{b,c},{a,d},{b,d},{c,d}},set) )

///

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
