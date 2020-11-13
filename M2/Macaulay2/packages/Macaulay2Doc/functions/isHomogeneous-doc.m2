--- status: DRAFT
--- author(s): MES
--- notes: some changes Sept 2018 LS

document { 
     Key => {isHomogeneous,
	  (isHomogeneous,Ring),
	  (isHomogeneous,ChainComplexMap),
	  (isHomogeneous,Matrix),
	  (isHomogeneous,RingElement),
	  (isHomogeneous,Module),
	  (isHomogeneous,ChainComplex),
	  (isHomogeneous,RingMap),
	  (isHomogeneous,Vector),
	  (isHomogeneous,Ideal)},
     Headline => "whether something is homogeneous (graded)",
     Usage => "isHomogeneous x",
     Inputs => {
	  "x" => {"a ", TO Ring, ", ",
	       TO RingElement, ", ",
	       TO Vector, ", ",
	       TO Matrix, ", ",
	       TO Ideal, ", ",
	       TO Module, ", ",
	       TO RingMap, ", ",
	       TO ChainComplex, ", or ",
	       TO ChainComplexMap}
	  },
     Outputs => {
	  Boolean => {"whether ", TT "x", " is homogeneous."}
	  },
     EXAMPLE {
	  "isHomogeneous(ZZ)",
	  "isHomogeneous(ZZ[x,y])",
	  "isHomogeneous(ZZ[x,y]/(x^3-x^2*y+3*y^3))",
	  "isHomogeneous(ZZ[x,y]/(x^3-y-3))"
	  },
     PARA{},
     "Quotients of multigraded rings are homogeneous, if the ideal is homogeneous.",
     EXAMPLE {
          "R = QQ[a,b,c,Degrees=>{{1,1},{1,0},{0,1}}];",
	  "I = ideal(a-b*c);",
	  "isHomogeneous I",
	  "isHomogeneous(R/I)",
	  "isHomogeneous(R/(a-b))"
	  },
     PARA{},
     "Polynomial rings over polynomial rings are multigraded.",
     EXAMPLE lines ///
	  A = QQ[a]
	  B = A[x]
	  degree x
	  degree a_B
     	  isHomogeneous B
     ///,
     PARA{},
     "A matrix is homogeneous if each entry is homogeneous of such a degree that the matrix has a well-defined degree.",
     EXAMPLE {
	  "S = QQ[a,b];",
	  "F = S^{-1,2}",
     	  "isHomogeneous F",
	  "G = S^{1,2}",
	  "phi = random(G,F)",
	  "isHomogeneous phi",
	  "degree phi"
	  },
     PARA{},
     "Modules are homogeneous if their generator and relation matrices are homogeneous.",
     EXAMPLE {
	  "M = coker phi",
	  "isHomogeneous(a*M)",
     	  "isHomogeneous((a+1)*M)"
	  },
     PARA{},
    "Note that no implicit simplification is done. Consider the following cautionary example.",
     EXAMPLE {
	 "R = QQ[x]",
	 "isHomogeneous ideal(x+x^2, x^2)"
	 },
     Caveat => {"No computation on the generators and relations is performed.
	 For example, if inhomogeneous generators of a homogeneous ideal are given, then the return value is ", TO false, "."},
     PARA{},
     SeeAlso => {degree, degrees, homogenize, "graded and multigraded polynomial rings", "graded modules", prune}
     }

TEST ///
isHomogeneous (ZZ/7)
isHomogeneous (ZZ/7[x])
isHomogeneous (ZZ/7[x]/(x^2-1))
isHomogeneous ZZ
A = QQ[a,b,c]
B = A[x,y]
isHomogeneous B
isHomogeneous ideal(a*x+y,y^3-b*x^2*y)
///
