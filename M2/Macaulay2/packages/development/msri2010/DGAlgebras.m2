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
     A#(symbol ring) = R;
     A#(symbol vars) = toList (T_1..T_(#degList));
     A#(symbol diff) = {};
     A#(symbol algebra) = R[A.vars, Degrees => degList, SkewCommutative => select(toList(0..(#degList-1)), i -> odd degList#i)];
     A#(symbol degreeList) = degList;
     -- should verify that the differential is indeed of degree -1
     new DGAlgebra from A
)

polyDiffMonomial = method()
polyDiffMonomial(DGAlgebra,RingElement) := (A,m) -> (
  -- uses the Leibniz rule to compute the differential of a traditional monomial
  expList := flatten exponents m;
  prevTerm := -1;
  dgSign := 1;
  diffList := apply(#expList, l -> if (expList#l != 0) then ( if (prevTerm == -1) then dgSign = 1 else dgSign = (-1)^((A.degreeList)#prevTerm);
	                                                      prevTerm = l;
							      dgSign*(m // (A.algebra_l)^((expList#l)))*expList#l*(A.algebra_l)^((expList#l)-1)*((A.diff)#l) )
					               else 0_(A.algebra) );
  sum diffList
)

polyDifferential = method()
polyDifferential(DGAlgebra,ZZ) := (A,n) -> (
  
  coefficients(dM, Monomials => flatten entries basis({6,0},A.algebra))
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
R = ZZ/101[x,y,z,w]/ideal{x^3,y^4,z^5}
A = dgAlgebra(R,{1,1,1,1,2,2,2})
A.diff = {x,y,z,w,x^2*T_1,y^3*T_2,z^4*T_3}
dM = polyDiffMonomial(A,T_1*T_6^3)
flatten entries basis({6,0},A.algebra)
coefficients(dM, Monomials => flatten entries basis({6,0},A.algebra))
A.algebra_0
help basis

R = ZZ/101[a,b]
S = R[c,d]
basis(2,S)

break
