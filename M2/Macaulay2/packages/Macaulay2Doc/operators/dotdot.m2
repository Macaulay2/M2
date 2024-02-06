-- -*- coding: utf-8 -*-
--		Copyright 1993-2009 by Daniel R. Grayson

undocumented {
     (symbol .., InfiniteNumber, InfiniteNumber),
     (symbol .., ZZ, InfiniteNumber),
     (symbol .., InfiniteNumber, ZZ),
     (symbol ..<, InfiniteNumber, InfiniteNumber),
     (symbol ..<, ZZ, InfiniteNumber),
     (symbol ..<, InfiniteNumber, ZZ)
     }

document { Key => symbol .., 
     Headline => "a binary operator, used for sequences of consecutive items",
     PARA{
	  "The most confusing thing about this operator, in all its guises, is that it is not a syntactic
	  construction, and so the resulting sequences do not splice themselves into
	  enclosing lists, as in each of the following examples."
	  },
     EXAMPLE lines ///
     {10..10}
     {10..8}
     {3..5,8..10}
     ///,
     PARA {"  Use ", TO "splice", " to fix that."},
     EXAMPLE lines ///
     splice {3..5,8..10}
     ///,
     PARA { "If a type of list, instead of a sequence, is desired, use ", TO "toList", " or the operator ", TO "new", "."},
     EXAMPLE lines ///
     0..5
     toList (0..5)
     new Array from 0..5
     new Sum from 0..5
     ///,
     PARA{
	  "The operator can be used with sequences or lists, whose elements are of various types, to produce rectangular
	  intervals.",
	  },
     EXAMPLE lines ///
     (0,0)..(1,3)
     p_(0,a) .. p_(1,c)
     p_(1,1) .. q_(2,2)
     ///,
     PARA { "Use ", TO "..<", " instead to get a sequence that stops short of the endpoint." },
     SeeAlso => {"polynomial rings", "subscripted variables", "..<"}
     }
document { Key => symbol ..<, 
     Headline => "a binary operator, used for sequences of consecutive items, not including the endpoint",
     PARA{
	  "The most confusing thing about this operator, in all its guises, is that it is not a syntactic
	  construction, and so the resulting sequences do not splice themselves into
	  enclosing lists, as in each of the following examples."
	  },
     EXAMPLE lines ///
     {10..<11}
     {10..<8}
     {3..<5,8..<10}
     ///,
     PARA {"  Use ", TO "splice", " to fix that."},
     EXAMPLE lines ///
     splice {3..<5,8..<10}
     ///,
     PARA { "If a type of list, instead of a sequence, is desired, use ", TO "toList", " or the operator ", TO "new", "."},
     EXAMPLE lines ///
     0..<5
     toList (0..<5)
     new Array from 0..<5
     new Sum from 0..<5
     ///,
     PARA{
	  "The operator can be used with sequences or lists, whose elements are of various types, to produce rectangular
	  intervals.",
	  },
     EXAMPLE lines ///
     (0,0)..<(2,3)
     p_(0,a) ..< r_(2,c)
     ///,
     PARA { "Use ", TO "..", " instead to get a sequence that does not stop short of the endpoint." },
     SeeAlso => {"polynomial rings", "subscripted variables", ".."}
     }
document { Key => {(symbol .., List, List), (symbol .., Sequence, Sequence)},
     Headline => "rectangular sequences of consecutive lists and sequences",
     Usage => "s .. t",
     Inputs => {"s","t"},
     Outputs => {
	  {"the rectangular sequence of consecutive lists of the same length, from ", TT "s", " to ", TT "t", ", in lexicographic order, inclusive,
	       obtained by applying the operator ", TO "..", " recursively"}
	  },
     EXAMPLE lines ///
     {a,1} .. {c,3}
     {1} .. {4}
     ///
     }
document { Key => {(symbol ..<, List, List), (symbol ..<, Sequence, Sequence)},
     Headline => "rectangular sequences of consecutive lists and sequences",
     Usage => "s ..< t",
     Inputs => {"s","t"},
     Outputs => {
	  {"the rectangular sequence of consecutive lists of the same length, from ", TT "s", " to ", TT "t", ", in lexicographic order, not
	       including the endpoint, obtained by applying the operator ", TO "..<", " recursively"}
	  },
     EXAMPLE lines ///
     {a,1} ..< {d,4}
     {1} ..< {5}
     ///
     }
document { Key => (symbol .., IndexedVariable, IndexedVariable),
     Usage => "s .. t",
     Headline => "sequences of consecutive indexed variables",
     Inputs => {"s","t"},
     Outputs => {
	  {"the sequence of indexed variables with the same base and with consecutive subscripts, inclusive"}
	  },
     EXAMPLE lines ///
     x_1
     x_1 .. x_10
     x_(a,1) .. x_(b,3)
     ///
     }
document { Key => (symbol ..<, IndexedVariable, IndexedVariable),
     Usage => "s ..< t",
     Headline => "sequences of consecutive indexed variables",
     Inputs => {"s","t"},
     Outputs => {
	  {"the sequence of indexed variables with the same base and with consecutive subscripts, not including the endpoint"}
	  },
     EXAMPLE lines ///
     x_1
     x_(a,1) ..< z_(c,3)
     ///
     }
document { Key => (symbol .., Symbol, Symbol),
     Usage => "s .. t",
     Headline => "sequences of consecutive symbols",
     Inputs => {"s","t"},
     Outputs => {
	  {"the sequence of consecutive symbols useful as variables in polynomial rings, from ", TT "s", " to ", TT "t", ", inclusive,
	       as determined by the ", TO (vars,ZZ)}
	  },
     EXAMPLE lines ///
     x .. z
     x .. C
     ///
     }
document { Key => (symbol ..<, Symbol, Symbol),
     Usage => "s ..< t",
     Headline => "sequences of consecutive symbols",
     Inputs => {"s","t"},
     Outputs => {
	  {"the sequence of consecutive symbols useful as variables in polynomial rings, from ", TT "s", " to ", TT "t", ", 
	       not including the endpoint,
	       as determined by the ", TO (vars,ZZ)}
	  },
     EXAMPLE lines ///
     x ..< z
     x ..< C
     ///
     }
document { Key => (symbol .., ZZ, ZZ),
     Usage => "s .. t",
     Headline => "sequences of consecutive integers",
     Inputs => {"s","t"},
     Outputs => {
	  {"the sequence of consecutive integers from ", TT "s", " to ", TT "t", ", inclusive"}
	  },
     EXAMPLE lines ///
     1 .. 10
     ///
     }
document { Key => (symbol ..<, ZZ, ZZ),
     Usage => "s ..< t",
     Headline => "sequences of consecutive integers",
     Inputs => {"s","t"},
     Outputs => {
	  {"the sequence of consecutive integers from ", TT "s", " to ", TT "t", ", not including the endpoint"}
	  },
     EXAMPLE lines ///
     0 ..< 10
     ///,
     PARA {
	  "The indices valid for a list of length ", TT "n", " can be obtained with ", TT "0 ..< n", "."
	  },
     EXAMPLE lines ///
     x = {a,c,e,f}
     0 ..< #x
     apply(0 ..< #x, i -> x#i)
     ///
     }

document { Key => (symbol .., String, String),
     Headline => "a sequence of consecutive strings",
     Usage => "s .. t",
     Inputs => {"s","t"},
     Outputs => {
	  {"the sequence of consecutive strings of the same length, from ", TT "s", " to ", TT "t", ", in lexicographic order, inclusive"}
	  },
     EXAMPLE lines ///
     "a" .. "z"
     "aa" .. "ce"
     "aaa" .. "abc"
     "佖" .. "佥"
     ///,
     SeeAlso => { (symbol ..<, String, String) }
     }
document { Key => (symbol ..<, String, String),
     Headline => "a sequence of consecutive strings, not including the endpoint",
     Usage => "s ..< t",
     Inputs => {"s","t"},
     Outputs => {
	  {"the sequence of consecutive strings of the same length, from ", TT "s", " to ", TT "t", ", in lexicographic order, inclusive"}
	  },
     EXAMPLE lines ///
     "a" ..< "z"
     "aa" ..< "ce"
     "aaa" ..< "bcd"
     "佖" ..< "佥"
     ///,
     SeeAlso => { (symbol .., String, String) }
     }

document { Key => (symbol .., RingElement, RingElement),
     Headline => "a sequence of consecutive generators of a polynomial ring",
     Usage => "s .. t",
     Inputs => {"s","t"},
     Outputs => {
	  {"the sequence of consecutive generators of a polynomial ring, from ", TT "s", " to ", TT "t", ", inclusive"}
	  },
     EXAMPLE lines ///
     R = QQ[a..z]
     b .. i
     plus oo
     ///,
     PARA {
	  "Warning: former behavior involved making the names of the generators consecutive, so the results in the 
	  next example differ from those given before."
	  },
     EXAMPLE lines ///
     R = QQ[e,d,c,b,a,X_1,y,X_2]
     e .. a
     X_1 .. X_2
     ///,
     PARA {
	  "Warning: since former behavior involved only the names of the generators, there was no requirement
	  that ", TT "s", " and ", TT "t", " be in the same ring, whereas now there is."
	  }
     }
document { Key => (symbol ..<, RingElement, RingElement),
     Headline => "a sequence of consecutive generators of a polynomial ring",
     Usage => "s ..< t",
     Inputs => {"s","t"},
     Outputs => {
	  {"the sequence of consecutive generators of a polynomial ring, from ", TT "s", " to ", TT "t", ", not including the endpoint"}
	  },
     EXAMPLE lines ///
     R = QQ[a..z]
     b ..< i
     plus oo
     ///
     }
