--- status: TODO
--- author(s): 
--- notes: 

document { 
     Key => {part, (part,ZZ,RingElement), (part,List,RingElement), (part,Sequence,RingElement)},
     Headline => "sum of monomials of a polynomial of a given degree(s)",
     SYNOPSIS (
	  Usage => "part(d,F)\npart_d F",
	  Inputs => {
	       "d" => ZZ => {"or a ", TO List, " of integers denoting a (multi-) degree"},
	       "F" => RingElement => "an element in a polynomial ring"
	       },
	  Outputs => {
	       RingElement => {"the degree ", TT "d", " part of the polynomial ", TT "F"}
	       },
	  "If the polynomial ring is singly graded (the default case), then d may be an integer
	  denoting this degree.",
	  EXAMPLE {
	       "R = QQ[a..d]",
	       "f = (a^2-b-1)*(c^3-b*d-2)",
	       "part(3,f)"
	       },
	  "Here is an alternate syntax.",
	  EXAMPLE {
	       "part_3 f"
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
	       "s" => Sequence => {"a sequence of degrees or multi-degrees"},
	       "F" => RingElement => "an element in a polynomial ring"
	       },
	  Outputs => {
	       Sequence => {"a sequence of the degree ", TT "d", " parts of the polynomial ", TT "F", " where ", TT "d", " ranges over
		    the elements of ", TT "s"}
	       },
	  EXAMPLE lines ///
	       f = (1+x+y)^3
	       part(1..3, f)
	  ///
	  ),
     SeeAlso => {degree, 
	  -- Mike wanted this: "multigradings",
	  parts}
     }
