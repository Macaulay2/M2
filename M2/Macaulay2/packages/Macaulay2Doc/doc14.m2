--		Copyright 2008 by Daniel R. Grayson

document {
     Key => {
	  (map,Module,Module,RingMap,Matrix),
	  (map,Module,Module,RingMap,List),
	  (map,Module,Nothing,RingMap,Matrix),
	  (map,Module,Nothing,RingMap,List),
	  (map,Module,RingMap)},
     Headline => "homomorphism of modules over different rings",
     Usage => "g = map(M,N,p,f)\ng = map(M,,p,f)\ng = map(M,p)",
     Inputs => { "M", 
	  "N" => {"or ", TO "null", ""},
	  "p" => {"from the ring of ", TT "N", " to the ring of ", TT "M"}, 
	  "f" => {"to the ring of ", TT "M", ", from the cover of ", TT "N", " tensored with the ring of ", TT "M", " along ", TT "p", ".
	       Alternatively, ", TT "f", " can be represented by its doubly nested list of entries."},
	  Degree => List => {
	       "a list of integers of length equal to the degree length of the ring of ", TT "M", ", providing the degree of ", TT "g", ".
	       By default, the degree of ", TT "g", " is zero."
	       }
	  },
     Outputs => {
	  "g" => Matrix => {"the homomorphism to M from N defined by f"}
	  },
     EXAMPLE lines ///
     R = QQ[x,y]
     p = map(R,QQ)
     f = matrix {{x-y, x+2*y, 3*x-y}};
     kernel f
     g = map(R^1,QQ^3,p,f)
     g === map(R^1,QQ^3,p,{{x-y, x+2*y, 3*x-y}})
     isHomogeneous g
     kernel g
     coimage g
     rank oo
     ///,
     PARA { "If the module ", TT "N", " is replaced by ", TO "null", ",
	  which is entered automatically between consecutive commas,
	  then a free module will be used for ", TT "N", ",
	  whose degrees are obtained by lifting
	  the degrees of the cover of the source of ", TT "g", ", minus the degree of ", TT "g", ", along the degree map of ", TT "p" },
     EXAMPLE lines ///
     g2 = map(R^1,,p,f,Degree => {1})
     g === g2
     ///,
     PARA {
	  "If N and f are both omitted, along with their commas, then for ", TT "f", " the matrix of generators of M is used."
	  },
     EXAMPLE lines ///
     M' = image f
     g3 = map(M',p,Degree => {1})
     isHomogeneous g3
     kernel g3
     oo == kernel g
     ///,
     PARA { "The degree of the homomorphism enters into the determination of its homogeneity." },
     EXAMPLE lines ///
     R = QQ[x, Degrees => {{2:1}}];
     M = R^1
     S = QQ[z];
     N = S^1
     p = map(R,S,{x},DegreeMap => x -> join(x,x))
     isHomogeneous p
     f = matrix {{x^3}}
     g = map(M,N,p,f,Degree => {3,3})
     isHomogeneous g
     kernel g
     coimage g
     ///,
     SeeAlso => { (map,Ring,Ring,List), isHomogeneous, (kernel,Matrix), (coimage,Matrix) }
     }

document {
     Key => {(quotientRemainder,RingElement,RingElement),
	  (quotientRemainder,InexactNumber,RingElement), (quotientRemainder,RingElement,InexactNumber),
	  (quotientRemainder,Number,RingElement), (quotientRemainder,RingElement,Number),
	  (quotientRemainder,ZZ,ZZ)},
     Headline => "quotient and remainder",
     Usage => "(q,r) = quotientRemainder(f,g)",
     Inputs => {"f","g"},
     Outputs => {
	  "q" => RingElement => {"the quotient for the division of ", TT "f", " by ", TT "g"},
	  "r" => RingElement => {"the remainder for the division of ", TT "f", " by ", TT "g"}
	  },
     EXAMPLE lines ///
     R = QQ[x,y];
     (q,r) = quotientRemainder(x^10+5,x-2);
     q
     r
     q*(x-2)+r
     ///
     }

document { Key => (symbol **,Matrix,RingElement),
     Usage => "f ** r",
     Inputs => {"f","r"},
     Outputs => {{"the tensor product of ", TT "f", " with ", TT "r"}},
     EXAMPLE lines ///
     f = matrix "2,3,4;5,6,7"
     f ** 10
     ///,
     PARA { "When the ring element is homogeneous, the degrees of the source module can change, which is
	  what makes this operation different from scalar multiplication." },
     EXAMPLE lines ///
     QQ[x,y]
     f = matrix "x,y"
     g = f ** y^7
     h = f * y^7
     degrees g
     degrees h
     ///
     }

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
