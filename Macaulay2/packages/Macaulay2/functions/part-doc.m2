--- status: TODO
--- author(s): 
--- notes: 

document { 
     Key => {part, (part,ZZ,RingElement), (part,List,RingElement)},
     Headline => "sum of monomials of a polynomial of a given degree",
     Usage => {"part(d,F), ", EM "or", " part_d F"},
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
     "An alternate syntax is as follows",
     EXAMPLE {
	  "part_3 f"
	  },
     "In multigraded rings, degrees are lists of integers.",
     EXAMPLE {
	  "R = QQ[a..d,Degrees=>{{1,0},{0,1},{1,-1},{0,-1}}]",
	  "F = a^3 + (b*d+1)^2",
	  "part_{0,0} F"
	  },
     "In polynomial rings over other polynomial rings, variables in the 
     coefficient ring have degree 0.",
     EXAMPLE {
	  "A = QQ[a,b,c]",
	  "B = A[x,y]",
	  "degree(a*x)",
	  "part_1 (a*x+b*y-1)^3"
	  },
     SeeAlso => {degree, "multigradings"}
     }
