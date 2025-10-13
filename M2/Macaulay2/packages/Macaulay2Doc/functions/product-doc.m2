document {
     Key => product,
     TT "product", " provides the product of the members of a list or set,
     optionally with a function applied to each one.",
     Subnodes => {
	 TO (product, List),
	 TO (product, ZZ, Function),
	 TO (product, VisibleList, Function),
	 TO (product, VisibleList, VisibleList, Function),
         },
     }
document {
     Key => (product, List),
     Headline => "product of elements",
     TT "product v", " yields the product of the elements in the list v.",
     PARA{},
     EXAMPLE "product {1,2,3,4,5}"
     }
document {
     Key => (product, VisibleList, VisibleList, Function),
     Headline => "product of results of applying a function pairwise",
     TT "product(v,w,f)", " yields the product of the results obtained by
     applying ", TT "f", " to each of the pairs ", TT "(i,j)", " of elements from
     the lists ", TT "v", " and ", TT "w", ", which should be of the same length.",
     PARA{},
     EXAMPLE {
	  "M = monoid [x,y,z];",
      	  "product({2,3,4},{x,y,z},(i,j)->j^i)",
	  },
     SeeAlso => "product"
     }
document {
     Key => (product, VisibleList, Function),
     Headline => "product of values of a function",
     TT "product(v,f)", " yields the product of the expressions obtained by
     applying ", TT "f", " to each of the elements of the list or sequence ", TT "v", ".",
     PARA{},
     EXAMPLE "product(1 .. 5, i -> i^2)",
     SeeAlso => "product"
     }
document {
     Key => (product, ZZ, Function),
     Headline => "product of consecutive values of a function",
     TT "product(n,f)", " compute the product ", TT "f(0) * f(1) * ... * f(n-1)", ".",
     PARA{},
     EXAMPLE "product(5, i -> 2*i+1)",
     SeeAlso => "product"
     }
document {
     Key => (product, VirtualTally),
     Headline => "product of elements",
     TT "product v", " yields the product of the elements in the tally ", TT "v", ".",
     PARA{},
     EXAMPLE {
	  "a = tally{2,2,2,2,2,3,3,3,5,5}",
      	  "product a",
	  },
     SeeAlso => "product"
     }
