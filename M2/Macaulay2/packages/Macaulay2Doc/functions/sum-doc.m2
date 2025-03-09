document {
     Key => sum,
     Headline => "compute the sum",
     TT "sum", " provides the sum of the members of a list, set,
     or chain complex, optionally with a function applied to each one.",
     Subnodes => {
	 TO (sum, List),
	 TO (sum, ZZ, Function),
	 TO (sum, VisibleList, Function),
	 TO (sum, VisibleList, VisibleList, Function),
         },
     }

document {
     Key => (sum, List),
     Headline => "sum the elements of a list",
     TT "sum v", " yields the sum of the elements in the list ", TT "v", ".",
     PARA{},
     EXAMPLE "sum {1,2,3,4,5}",
     PARA {
	  "The sum of an empty list is the integer 0."
	  },
     EXAMPLE lines ///
     sum {}
     class oo
     ///,
     PARA {
	  "When summing a possibly empty list of elements from a ring, one may
	  use ", TO "promote", " to ensure the result is always in the same ring."
	  },
     EXAMPLE lines ///
     R = QQ[x_1 .. x_10];
     f = n -> sum for i from 1 to n list x_i;
     f 4
     f 0
     class oo
     g = n -> promote(sum for i from 1 to n list x_i, R);
     g 10
     g 0
     class oo
     ///,
     SeeAlso => "sum"
     }
document {
     Key => (sum, VisibleList, VisibleList, Function),
     Headline => "sum results of applying a function pairwise",
     TT "sum(v,w,f)", " yields the sum of the results obtained by
     applying ", TT "f", " to each of the pairs ", TT "(i,j)", " of elements from
     the lists or sequences ", TT "v", " and ", TT "w", ", which should be of
     the same length.",
     PARA{},
     EXAMPLE {
	  "R = ZZ[x,y,z];",
      	  "sum({2,3,4},{x,y,z},(i,j)->j^i)",
	  },
     SeeAlso => "sum"
     }
document {
     Key => (sum, VisibleList, Function),
     Headline => "sum results of applying a function",
     TT "sum(v,f)", " yields the sum of the expressions obtained by
     applying ", TT "f", " to each of the elements of the list or sequence ", TT "v", ".",
     PARA{},
     EXAMPLE "sum(1 .. 10, i -> i^2)",
     SeeAlso => "sum"
     }
document {
     Key => (sum, ZZ, Function),
     Headline => "sum consecutive values of a function",
     TT "sum(n,f)", " computes the sum ", TT "f(0) + f(1) + ... + f(n-1)", ".",
     PARA{},
     EXAMPLE "sum(10, i -> i^2)",
     SeeAlso => {"product", "plus", "times"}
     }
document {
     Key => (sum, VirtualTally),
     Headline => "sum of elements",
     TT "sum v", " yields the sum of the elements in the tally ", TT "v", ".",
     PARA{},
     EXAMPLE {
	  "a = tally{1,1,1,1,1,10,10,10,100,100}",
      	  "sum a",
	  },
     SeeAlso => "product"
     }
