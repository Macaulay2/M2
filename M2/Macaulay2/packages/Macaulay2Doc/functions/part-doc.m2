--- status: TODO
--- author(s): 
--- notes: 

document {
     Key => part, 
     Headline => "select terms of a polynomial by degree or weight",
     }

document {
     Key => {(part,ZZ,ZZ,VisibleList,RingElement),
	  (part,InfiniteNumber,InfiniteNumber,RingElement),
	  (part,InfiniteNumber,InfiniteNumber,VisibleList,RingElement),
	  (part,InfiniteNumber,ZZ,RingElement),
	  (part,InfiniteNumber,ZZ,VisibleList,RingElement),
	  (part,Nothing,Nothing,RingElement),
	  (part,Nothing,Nothing,VisibleList,RingElement),
	  (part,Nothing,ZZ,RingElement),
	  (part,Nothing,ZZ,VisibleList,RingElement),
	  (part,ZZ,InfiniteNumber,RingElement),
	  (part,ZZ,InfiniteNumber,VisibleList,RingElement),
	  (part,ZZ,Nothing,RingElement),
	  (part,ZZ,Nothing,VisibleList,RingElement),
	  (part,ZZ,RingElement),
	  (part,ZZ,VisibleList,RingElement),
	  (part,ZZ,ZZ,RingElement)
	  },
     Usage => "part(lo,hi,wt,f)",
     Inputs => { "lo", "hi", "wt" => {"whose elements are integers (after splicing)"}, "f" },
     Outputs => { 
	  RingElement => { "the sum of those terms of ", TT "f", " whose weights, with respect to ", TT "wt", ", are in the range ", TT "lo..hi" }
	  },
     EXAMPLE lines ///
     R = QQ[x,y,z,Degrees=>{3,2,1}];
     f = (1+x+y+z)^3
     part(0,1,3:1,f)
     part(0,1,1..3,f)
     part(7,9,1..3,f)
     ///,
     PARA {
	  "If ", TT "wt", " is omitted, and the ring is singly graded, then the degrees of the variables are used as the weights."
	  },
     EXAMPLE lines ///
     gens R
     degree \ oo
     part(7,9,f)
     ///,
     PARA {
	  "If ", TT "lo", " or ", TT "hi", " is omitted, but not the corresponding comma, then there is no corresponding bound on the weights
	  of the terms provided."
	  },
     EXAMPLE lines ///
     part(7,,f)
     part(,3,f)
     part(,3,1..3,f)
     ///,
     PARA {
	  "The bounds may be infinite."
	  },
     EXAMPLE lines ///
     part(7,infinity,f)
     part(-infinity,3,f)
     part(-infinity,infinity,1..3,f)
     ///,
     PARA {
	  "If just one limit is provided, terms whose weight are equal to it are provided."
	  },
     EXAMPLE lines ///
     part(7,f)
     part(7,1..3,f)
     ///,
     PARA {
	  "For polynomial rings over polynomial rings, all of the variables participate."
	  },
     EXAMPLE lines ///
     S = QQ[a][x];
     g = (1+a+x)^3
     part(2,{1,1},g)
     part(2,{1,0},g)
     part(2,,{0,1},g)
     ///     
     }

document { 
     Key => {(part,List,RingElement), (part,Sequence,RingElement)},
     Headline => "sum of terms of a polynomial of a given degree(s)",
     SYNOPSIS (
	  Usage => "part(d,F)\npart_d F",
	  Inputs => { "d" => "of integers denoting a multidegree", "F" => "an element in a polynomial ring" },
	  Outputs => { RingElement => {"the degree ", TT "d", " part of the polynomial ", TT "F"} },
	  "If the polynomial ring is singly graded (the default case), then d may be an integer
	  denoting this degree.",
	  EXAMPLE {
	       "R = QQ[a..d]",
	       "f = (a^2-b-1)*(c^3-b*d-2)",
	       "part({3},f)"
	       },
	  "Here is an alternate syntax.",
	  EXAMPLE {
	       "part_{3} f"
	       },
	  "In multigraded rings, degrees are lists of integers.",
	  EXAMPLE {
	       "R = QQ[a..d,Degrees=>{{1,0},{0,1},{1,-1},{0,-1}}]",
	       "F = a^3 + (b*d+1)^2",
	       "part_{0,0} F"
	       },
	  "Polynomial rings over other polynomial rings are multigraded, by default.",
	  EXAMPLE {
	       "A = QQ[a,b,c]",
	       "B = A[x,y]",
	       "degree(a*x)",
	       "part_{2,2} (a*x+b*y-1)^3"
	       }
	  ),
     SYNOPSIS (
	  Usage => "part(s,F)",
	  Inputs => {
	       "s" => Sequence => {"a sequence of multidegrees"},
	       "F" => RingElement => "an element in a polynomial ring"
	       },
	  Outputs => {
	       Sequence => {"a sequence of the degree ", TT "d", " parts of the polynomial ", TT "F", " where ", TT "d", " ranges over
		    the elements of ", TT "s"}
	       },
	  EXAMPLE lines ///
	  R = QQ[x,y];
	  f = (1+x+y)^6
	  part(({1},{3},{5}), f)
	  ///,
	  PARA {
	       "Warning: if the ring is singly graded and the degrees to be selected are consecutive,
	       then it can be much faster to use ", TO (part,ZZ,ZZ,VisibleList,RingElement), " instead,
	       because the code used here selects the desired parts one at a time, making many passes
	       over the terms of ", TT "f", "."
	       }
	  ),
     SeeAlso => {degree, 
	  -- Mike wanted this: "multigradings",
	  parts}
     }
