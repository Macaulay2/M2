-- -*- coding: utf-8 -*-
newPackage("DGAlgebras",
     Authors => {
	  {Name => "Frank Moore"},
	  {Name => "Others"}
	  },
     DebuggingMode => true,
     Headline => "Data type for DG Algebras",
     Version => "0.1"
     )

export { DGAlgebra, dgAlgebra, algebra }

------------------------------------------------
-- Set DG algebra types and constructor functions. -- 
------------------------------------------------

DGAlgebra = new Type of MutableHashTable

dgAlgebra = method()
dgAlgebra(Ring,List) := (R,degList) -> (
     -- Input:  A ring, a list of degrees of the variables, and a list that defines the differential
     -- Output:  A hash table of type DGAlgebra
     A := new MutableHashTable;
     --A#(symbol ring) = newRing(R, Degrees => apply(degrees R, i -> 0));
     A#(symbol ring) = R;
     A#(symbol vars) = toList (T_1..T_(#degList));
     A#(symbol diff) = {};
     A#(symbol algebra) = (A.ring)[A.vars, Degrees => degList, SkewCommutative => select(toList(0..(#degList-1)), i -> odd degList#i)];
     A#(symbol degreeList) = degList;
     -- should verify that the differential is indeed of degree -1
     new DGAlgebra from A
)

koszulDGAlgebra(Ring) := (R) -> (
     A := dgAlgebra(R, toList ((numgens R):1));
     use A.ring;
     A.diff = gens R;
     A
)

koszulDGAlgebra(Ideal) := (I) -> (
     A := dgAlgebra(ring I, toList ((numgens R):1));
     use A.ring;
     A.diff = gens I;
     A
)

polyDiffMonomial = method()
polyDiffMonomial(DGAlgebra,RingElement) := (A,m) -> (
  -- uses the Leibniz rule to compute the differential of a traditional monomial
  expList := flatten exponents m;
  dgSign := 1;
  diffList := apply(#expList, l -> if (expList#l != 0) then ( dgSign = sum apply(l, i -> (A.degreeList)#i*expList#i);
	                                                      prevTerm = l;
							      firstDegList := take(expList, l);
							      lastDegList := drop(expList, l+1);
							      firstTerm := 1;
							      lastTerm := 1;
							      if (l != 0) then firstTerm = product apply(#firstDegList, i -> (A.algebra_i)^(firstDegList#i));
      							      if (l != (#expList - 1)) then lastTerm = product apply(#lastDegList, i -> (A.algebra_(l+i+1))^(lastDegList#i));
							      (expList#l)*(-1)^dgSign*firstTerm*((A.diff)#l)*(A.algebra_l)^((expList#l)-1)*lastTerm)
							      --(-1)^dgSign*firstTerm*((A.diff)#l)*(A.algebra_l)^((expList#l)-1)*lastTerm)
					               else 0_(A.algebra) );
  sum diffList
)

polyDifferential = method()
polyDifferential(DGAlgebra,ZZ) := (A,n) -> (
  sourceList := flatten entries basis({n,0},A.algebra);
  targetList := flatten entries basis({n-1,0},A.algebra);
  myMatrix := substitute(fold(apply(sourceList, i -> (coefficients(polyDiffMonomial(A,i), Monomials => targetList))#1), (i,j) -> i | j), R);
  map((A.ring)^(#targetList), (A.ring)^(#sourceList), entries myMatrix)
)

polyDifferential(DGAlgebra,RingElement) := (A,f) -> (
  sum apply(terms f, m -> polyDiffMonomial(A,m))
)

polyHomology = method()
polyHomology(DGAlgebra,ZZ) := (A,n) -> (
  dn := 0;
  if n == 0 then dn = map(R^0, R^1, 0) else dn = polyDifferential(A,n);
  dnplus1 := polyDifferential(A,n+1);
  prune homology(dn,dnplus1)
)

divPowDiffMonomial = method()
divPowDiffMonomial(DGAlgebra,RingElement) := (A,m) -> (
  -- uses the Leibniz rule to compute the differential of a divided powers monomial
  A  
)

--------------------
-- Documentation  --
--------------------

beginDocumentation()

doc ///
  Key
    DGAlgebra
  Headline
    Data types and basic functions on graphs used in algebra and algebraic geometry. 
  Description
    Text
      This package is used to construct graphs. 
///

end

restart
loadPackage "DGAlgebras"
debug DGAlgebras
R = ZZ/32003[x,y,z,w]/ideal{x^3,y^4,z^5}
A = dgAlgebra(R,{1,1,1,1,2,2,2})
B = dgAlgebra(R)
polyHomology(B,0)
use A.ring;
A.diff = {x,y,z,w,x^2*T_1,y^3*T_2,z^4*T_3}
d1 = polyDifferential(A,1)
d2 = polyDifferential(A,2)
d3 = polyDifferential(A,3)
d4 = polyDifferential(A,4)
d5 = polyDifferential(A,5)
d17 = polyDifferential(A,17);
d1*d2
d2*d3
d3*d4
polyHomology(A,7)
polyHomology(A,17)

use R
res(coker vars R, LengthLimit => 10)

R = ZZ/101[a,b]
S = R[c,d]
basis(2,S)

break
