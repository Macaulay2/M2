newPackage(
    "HomotopyLieAlgebra",                                                 
                Headline => "Homotopy Lie algebra",
                Version => "0.9",                                                
                Date => "October 19, 2021",                                        
                Authors => {                                                     
                    {Name => "David Eisenbud", Email => "de@msri.org", HomePage => "https://www.msri.org/~de"}
		    },
                DebuggingMode => false,
		PackageExports => {"DGAlgebras"}
                )                                      


export {"bracket",
	"bracketMatrix",
	"allgens", 
	"ad"}

-* Code section *-
homdeg = f -> first degree f 
intdeg = f -> last degree f
absdeg = m -> sum(listForm m)_0_0 -- number of factors of a monomial
isSquare = m -> max (listForm m)_0_0 >=2

allgens = method()
-*
allgens DGAlgebra := List => A -> (
    g := apply(gens((flattenRing A.natural)_0), 
               t -> sub(t, A.natural));
    sort(g, T -> homdeg T))
allgens(DGAlgebra, ZZ) := List => (A,d) -> select(allgens A, T -> homdeg T == d)		
*-
allgens DGAlgebra := List => A -> (
    flatten entries (vars coefficientRing A.natural | vars A.natural)
    )

allgens(DGAlgebra, ZZ) := List => (A,d) -> select(allgens A, T -> homdeg T == d)		

--if V is a variable with a coefficient, we want to extract the index of the variable!
ind = V -> (keys ((keys standardForm V)_0))_0

pairing1 = method()
pairing1(List, RingElement) := RingElement => (L,M) -> (
    --L = {U,V}, where U,V are (dual) variables of A1
    --M is a monomial of absdeg 2 (possibly with nontrivial coef) in A1
    --return 0 unless M == r*x*y, and U,V = x,y or y,x, where r is in CoefficientRing A1.
    --Returns:
    --r(contract(V,x)*contract(U,y)+(-1)^((deg U)*(deg V))*contract(U,x)*contract(V,y).
    --Note that the monomials xy in diff(A, T_) are always written with increasing index.
    --Thus if index V > index U then <V,x> = 0, so the result is (-1)^(deg*deg)<u,x><v,y>
    (U,V) := (L_0,L_1);
    A1 := ring U;
    M2 := contract(V,M);
    if M2 == 0  then return 0_A1;
    Mcoef := contract(U,M2);
    if Mcoef == 0 then return 0_A1;
    --at this point M = cxy, with index y >= index x
    if isSquare M then if  homdeg U%2==0 then return 2*Mcoef else return 0_A1;
    if ind V < ind U then Mcoef else
    (-1)^((homdeg U)*(homdeg V)) * Mcoef
    )

pairing1(List, RingElement,String) := RingElement => (L,M,s) -> (
    --this is an alternate version, maybe faster?
    
    --L = {U,V}, where U,V are (dual) variables of A1
    --M is a monomial of absdeg 2 (possibly with nontrivial coef) in A1
    --return 0 unless M == r*x*y, and U,V = x,y or y,x, where r is in CoefficientRing A1.
    --Assumes ind x < ind y
    --Returns:
    --r(contract(V,x)*contract(U,y)+(-1)^((deg U)*(deg V))*contract(U,x)*contract(V,y).
    --Note that the monomials xy in diff(A, T_) are always written with increasing index.
    --Thus if index V > index U then <V,x> = 0, so the result is (-1)^(deg*deg)<u,x><v,y>
    (U,V) := (L_0,L_1);
    Mcoef := contract(U*V,M);
    --treat the case where the sign is +:
    if ((homdeg U)*(homdeg V)) % 2 == 0 then
       if U == V then return 2*Mcoef else return Mcoef;
    --now both are odd, sign is -
    if U == V then return 0;
    if ind V<ind U then return Mcoef else return -Mcoef
    )

///
debug HomotopyLieAlgebra
kk = ZZ/101
S = kk[x,y]
R = S/ideal(x^2,y^2,x*y)
lastCyclesDegree = 4
KR = koszulComplexDGA(ideal R)
A = acyclicClosure(KR, EndDegree => lastCyclesDegree)
--this is a wrong test: we need to assume that M = xy with ind x < ind y.
N = flatten for u in allgens(A,1) list for v in allgens(A,1) list (
    if ind u < ind v then M = u*v else M = v*u;
    (u,v,pairing1({u,v},M) == pairing1({u,v},M,"")))
N/last
--some of the equalities fail, but the checks pass with both versions of the pairing. ???
t = allgens (A,1)
L = {t_1,t_0}, M = t_0*t_1
pairing1(L,M)
pairing1(L,M,"")
///

pairing = method()
pairing(List, RingElement) := RingElement => (L,M) -> (
    --L = {U,V}, where U,V are scalar linear combinations of generators u, v of A1,
    --all of the same homdeg
    --M is an element of A1
    --returns the sum of the values of pairing1({u,v},m)
    (U,V) := (L_0,L_1);
    MM := select(terms M, m -> absdeg m == 2);
    UU := terms U;
    VV := terms V;
    sum apply(UU, 
	u-> sum apply(VV, 
	    v -> sum apply(MM, 
		M'-> pairing1({u,v},M'))))
     )

pairing(List, RingElement, String) := RingElement => (L,M,s) -> (
    --L = {U,V}, where U,V are scalar linear combinations of generators u, v of A1,
    --all of the same homdeg
    --M is an element of A1
    --returns the sum of the values of pairing1({u,v},m)
    (U,V) := (L_0,L_1);
    MM := select(terms M, m -> absdeg m == 2);
    UU := terms U;
    VV := terms V;
    sum apply(UU, 
	u-> sum apply(VV, 
	    v -> sum apply(MM, 
		M'-> pairing1({u,v},M',"s"))))
     )

bracket = method()
bracket (DGAlgebra, List) := Matrix => (A,L) ->(
    --L = {U,V}, where U,V are scalar linear combinations of (dual) generators of A.natural
    --of homological degrees d-1,e-1, regarded as elements of Pi_d,Pi_e.
    --returns [U,V] 
    --as an element of Pi_(d+e), represented as a sum of the dual generators
    --of A.natural of degree i+j-1 
    if L_0 == 0 or L_1 ==0 then return 0_(A.natural);

    if not A.cache#?"maps" then A.cache#"maps" = (
    (A',toA') := flattenRing A.natural;
    fromA' := toA'^(-1);
    g' := sort(gens A', t->homdeg t); -- put the vars in order of homological degree
    B := coefficientRing A'[g', Degrees => g'/degree]; --this will be A1 below.
    f := (map(B,A')*toA');
    g := fromA'*(map(A',B));
    (f,g));

    (toA1,fromA1) := A.cache#"maps";
    A1 := target toA1;
    (U1,V1) := (toA1 L_0,toA1 L_1); 
    g1 := select(gens A1, T->homdeg T == homdeg U1 + homdeg V1 + 1);
    sum apply(g1, T ->(
	dT := toA1 diff(A, fromA1 T);
	fromA1 ((-1)^(homdeg V1)*T*pairing({U1,V1}, dT,""))
	))
    )

bracketMatrix = method()
bracketMatrix (DGAlgebra, ZZ,ZZ) := Matrix => (A,d,e) -> (
Pd := allgens(A,d-1); --dual basis of d-th homotopy group
Pe := allgens(A,e-1); --dual basis of e-th homotopy group
    matrix apply(Pd, T -> apply(Pe, T' -> bracket(A,{T,T'})))
	    )

bracket (DGAlgebra, List, RingElement) := Matrix => (A,L,T) ->(
    --L = {U,V}, where U,V are (dual) linear forms of A.natural
    --regarded as generators of Pi and , and T is a an element of A.natural
    --returns the action of [U,V] on T
    (-1)^(homdeg L_1)*pairing(L, diff(A,T),"")
	)

bracket(DGAlgebra, ZZ, ZZ) := HashTable => (A,d1,d2) -> (
    g1 := allgens(A, d1-1);
    g2 := allgens(A, d2-1);
    g3 := allgens(A, d1+d2-1);        
    print(#g1, #g2, #g3);
    print g3;
    hashTable flatten flatten apply(g1, 
	u -> apply(g2, 
	    v -> apply(g3, 
		T ->( ({u,v},T) => bracket(A,{u,v},T))
		)))
    )

bracket(DGAlgebra, ZZ, ZZ) := HashTable => (A,d1,d2) -> (
    g1 := allgens(A, d1-1);
    g2 := allgens(A, d2-1);
    g3 := allgens(A, d1+d2-1);        
    g3diff := apply(g3, T-> diff(A,T));
    print(#g1, #g2, #g3);
    print g3;
    hashTable flatten flatten apply(g1, 
	u -> apply(g2, 
	    v -> apply(g3diff, 
		dT ->( ({u,v},dT) =>
		    (-1)^(homdeg v)*pairing({u,v}, dT,"")
		))))
    )

ad = method()
ad(DGAlgebra, RingElement, ZZ) := Matrix => (A,U,e) ->(
    --U is a scalar linear combination of generators of A.natural all of the same 
    --homdeg, d-1 
    --regarded as an element of Pi_(d).
    --ad(U): Pi^e -> Pi^(d+e) is a linear transformation V-> [U,V].
    d := homdeg U + 1;
    md := matrix{allgens(A,d-1)};
    me := matrix{allgens(A, e-1)}; 
    mf := matrix{allgens(A, d + e - 1)};
    UU := contract(md, U);
    b := bracketMatrix(A,d,e);
    contract(transpose mf, UU*b)
    )

///
restart
loadPackage "HomotopyLieAlgebra"

   kk = ZZ/101
   S = kk[x,y,z]
   R = S/ideal(x^2,y^2,z^2-x*y,x*z, y*z)
   lastCyclesDegree = 4
   KR = koszulComplexDGA(ideal R)
   A = acyclicClosure(KR, EndDegree => lastCyclesDegree);
   d = 1
   e = 1
   U = sum (Gd = allgens(A,d-1))
   ad(A,U,1)
      
///

-* Documentation section *-
beginDocumentation()


doc ///
Key
 HomotopyLieAlgebra
Headline 
 Homotopy Lie Algebra of a surjective ring homomorphism
Description
  Text
   If R = S/I, K is the Koszul complex on the generators of I, and A is the DGAlgebra
   that is the acyclic closure of K, then the homotopy Lie algebra Pi of the map S -->> R
   is defined as in Briggs ****, with underlying vector space the graded dual
   of the space spanned by a given set of generators of A.
  Example
   S = ZZ/101[x,y]
   R = S/ideal(x^2,y^2,x*y)
   KR = koszulComplexDGA(ideal R)
  Text
   Since the acyclic closure is infinitely generated, we must specify 
   the maximum homological degree
   in which cycles will be killed
  Example
   lastCyclesDegree = 4
   A = acyclicClosure(KR, EndDegree => lastCyclesDegree)
  Text
   The evaluation of bracketMatrix(A,d,e) gives the matrix of values of [Pi^d,Pi^e]. Here
   we are identifying the vector space spanned by the generators of A with its graded dual
   by taking the generators produced by the algorithm in the DGAlgebras package to
   be self-dual.
  Example
   bracketMatrix(A,1,1)
   bracketMatrix(A,2,1)
   bracketMatrix(A,2,2)
  Text
   Note that bracketMatrix(A,d,e) is antisymmetric in d,e if one of them is even,
   and symmetric in d,e if both are odd
  Example
   bracketMatrix(A,1,1) - transpose bracketMatrix(A,1,1)
   bracketMatrix(A,2,1) + transpose bracketMatrix(A,1,2)
References 
 Briggs, Avramov
SeeAlso
///

doc ///
Key 
 bracket
 (bracket, DGAlgebra, List, RingElement) 
 (bracket, DGAlgebra, ZZ, ZZ)
 (bracket, DGAlgebra, List)
Headline
 Computes the Lie product
Usage
 F = bracket(A,L,T)
 F = bracket(A,L)
 H = bracket(A,d,e)
Inputs
 A:DGAlgebra
 L:List
  list of two generators of A
 T:RingElement
  generator of A
 d:ZZ
 e:ZZ
Outputs
 F:RingElement
  linear form in A 
 H:HashTable
  gives all the products between (dual) elements of degrees d,e
Description
  Text
   Given a factor ring R = S/I, we take
   A to be the acyclic closure, up to some degree n, of 
   the Koszul complex on the generators of I. The underlying algebra An := A.natural
   is thus a free algebra over S on generators T_i of various homological and internal
   degrees.
   
   The Homotopy Lie algebra of the map S ->> R is the graded dual of the space of linear forms
   Lin(A) of A. Since we obtain A together with a set of generators T_i, we identify the
   Lie algebra with space of linear forms, using the T_i as a self-dual basis. Thus we
   express the bracket product of two linear forms as another linear form. This inner product
   on the linear forms of degree d extends naturally to an inner product between
   Lin(A)**Lin(A) and the quadratic forms of A. This inner product has signs coming from
   the homological grading, and is the only delicate part of the implementation.
   
   The bracket function is the workhorse of this collection of routines. Suppose that f,g are
   three linear forms in the generators of A homogeneous in the homological grading,
   of homological degrees d-1, e-1 respectively, which we regard as dual basis elements
   of Pi^d and Pi^e, graded components of the homotopy Lie algebra Pi. We can compute
   the bracket product
   [f,g] as an element of Pi^{d+e}, or its action on a linear form F 
   that is homogeneous of homological degree d+e-1 via the
   inner product<f**g, d(F)_2>, where d is the differential of A. The calls
   bracket(A,{f,g}) and
   bracket(A, {f,g}, F) compute these products
   
   In the following example, we use the function 
   allgens(A,d) to list the generators of A of homological degree d:
  Example
   --   restart
   --   needsPackage "DGAlgebras"
   --   needsPackage "HomotopyLieAlgebra"
   kk = ZZ/101
   S = kk[x,y,z]
   R = S/ideal(x^2,y^2,z^2-x*y,x*z, y*z)
   lastCyclesDegree = 4
   KR = koszulComplexDGA(ideal R)
   A = acyclicClosure(KR, EndDegree => lastCyclesDegree);
   p2 = allgens(A,1) -- dual generators of Pi^2   
   p3 = allgens(A,2) -- dual generators of Pi^3
   a5 = allgens(A,4) -- generators of A of homological degree 5
   bracket(A, {sum p2, sum p3})
   bracket(A,{sum p2, sum p3}, sum a5)
  Text
   The other invocation of bracket produces a HashTable displaying all the
   bracket products of elements of Pi^d and Pi^e as functions on the generators
   of homological degree d+e-1 of A:
  Example
   H = bracket(A,2,3);
   #keys H
   H' = select(keys H, k->H#k != 0);
   H'
   H#(H'_0)
  Text
   From this we see that [T_5, T_6] sends T_37 to -1 in kk.
   
   Another, often simpler view of the pairing is given by @TO bracketMatrix@, where the
   rows and columns correspond to the generators of Pi^d and Pi^e, and the entries
   are the bracket products, interpreted as elements of Pi^{d+e}. Note the anti-symmetry,
   which holds when d or e are even and the symmetry in the case 
   both are odd.
  Example
   bracketMatrix(A,1,2)
   bracketMatrix(A,2,1)
   bracketMatrix(A,1,1)
SeeAlso
 allgens
 bracketMatrix
///

doc ///
Key
 bracketMatrix
 (bracketMatrix, DGAlgebra, ZZ, ZZ)
Headline
 Multiplication matrix of the homotopy Lie algebra
Usage
 M = bracketMatrix(A,d,e)
Inputs
 A:DGAlgebra
  first part of the acyclic closure of a Koszul complex
 d:ZZ
 e:ZZ
Outputs
 M:Matrix
  of linear forms in the generators of A
Description
  Text
   This function implements the multiplication table of the degree d and degree e
   components of the homotopy Lie algebra Pi. The entries of the matrix are linear
   forms of homological degree d+e+1, interpreted as generators of Pi^{d+e}. See
   @TO bracket@ for more details.
  Example
   kk = ZZ/101
   S = kk[x,y,z]
   R = S/ideal(x^2,y^2,z^2-x*y,x*z, y*z)
   lastCyclesDegree = 4
   KR = koszulComplexDGA(ideal R)
   A = acyclicClosure(KR, EndDegree => lastCyclesDegree);
   p1 = allgens(A,0) -- dual generators of Pi^1
   p2 = allgens(A,1) -- dual generators of Pi^3
   p3 = allgens(A,2) -- dual generators of Pi^4
   bracketMatrix(A,2,1)
SeeAlso
 bracket
 allgens
///

doc ///
Key
 allgens
 (allgens, DGAlgebra)
 (allgens, DGAlgebra, ZZ)
Headline
 List the generators of a given degree
Usage
 G = allgens A
 Gd = allgens(A,d)
Inputs
 A:DGAlgebra
 d:ZZ
Outputs
 G: List
  of all generators of A
 Gd:List
  of all generators of homological degree d.
Description
  Text
   The DGAlgebra is constructed as a polynomial ring over a ground ring that
   is already a polynomial ring, and allgens includes the generators of the
   subring.
   
   In invocations of @TO bracket@ and @TO bracketMatrix@ it is useful to refer to the
   generators by reference to the lists formed by allGens, rather than by trying to name
   them directly, since there is a confusion between generators of A.natural and
   generators of (flattenRing A.natural)_0.
   
  Example
   kk = ZZ/101
   S = kk[x,y,z]
   R = S/ideal(x^2,y^2,z^2-x*y,x*z, y*z)
   lastCyclesDegree = 1
   KR = koszulComplexDGA(ideal R)
   A = acyclicClosure(KR, EndDegree => lastCyclesDegree);
  Text
   This causes the generators of the acyclic closure to be computed up to homological degree
   lastCycleDegree+1, to kill the cycles in lastCycleDegree.
   If S is graded, then the generators of A.natural have degreeLength 2, with the first
   component the homological degree.
   
   Since A.natural is a polynomial ring over S, gens A.natural only lists the generators
   of homological degree >=1, whereas allgens (made with fl
  Example
   g = gens(A.natural)
   g/degree
   allgens A
   G3 = allgens(A,2)
   G3/degree
///

doc ///
Key
 ad
 (ad, DGAlgebra, RingElement, ZZ)
Headline
 matrix of the adjoint action
Usage
 M = ad(A,U,e)
Inputs
 A:DGAlgebra
 U:RingElement
  linear form in the generators of A
 e:ZZ
Outputs
 M:Matrix
  with entries in the ground field
Description
  Text
   The adjoint action of a scalar linear combination of the entries of allgens(A,d-1)
   U, regarded as an element of Pi^d, acts by bracket multiplcation with 
   source Pi^e and target Pi^{d+e}. The output is a matrix whose columns 
   correspond to a generalized row of the output of bracketMatrix.
   bracketmatrix
  Example
   kk = ZZ/101
   S = kk[x,y,z]
   R = S/ideal(x^2,y^2,z^2-x*y,x*z, y*z)
   lastCyclesDegree = 4
   KR = koszulComplexDGA(ideal R)
   A = acyclicClosure(KR, EndDegree => lastCyclesDegree);
   d = 1
   e = 1
   U = sum (Gd = allgens(A,d-1))
   ad(A,U,1)
  Text
   The columns of this matrix are the functionals that are the sum of the three
   rows of the bracket multiplication table:
  Example
   matrix{{1,1,1}}*bracketMatrix(A,d,e)
SeeAlso
 bracketMatrix
///

-*
-* Test section *-
TEST/// --graded skew symmetry:
kk = ZZ/101
S = kk[x,y]
R = S/ideal(x^2,y^2,x*y)
lastCyclesDegree = 4
KR = koszulComplexDGA(ideal R)
A = acyclicClosure(KR, EndDegree => lastCyclesDegree)
--
assert(bracketMatrix(A,1,1) - transpose bracketMatrix(A,1,1) == 0)
assert(bracketMatrix(A,2,1) + transpose bracketMatrix(A,1,2) == 0)
assert(bracketMatrix(A,2,2) + transpose bracketMatrix(A,2,2) == 0) 
assert(bracketMatrix(A,2,3) + transpose bracketMatrix(A,3,2) == 0) 
assert(bracketMatrix(A,3,3) - transpose bracketMatrix(A,3,3) == 0) 

///

///
restart
needsPackage "DGAlgebras"
loadPackage "HomotopyLieAlgebra"
check HomotopyLieAlgebra
///

TEST/// 
--gradedJacobi identity: 
--[U,[V,W]] = [[U,V],W] + (-1)^(1+homdeg U)*(1+homdeg V))* [V,[U,W]]

kk = ZZ/101
S = kk[x,y]
R = S/ideal(x^2,y^2,x*y)
lastCyclesDegree = 4
KR = koszulComplexDGA(ideal R)
A = acyclicClosure(KR, EndDegree => lastCyclesDegree)

L = for d from 2 to 3 list for e from 2 to 3 list for f from 2 to 3 list(
    Pid = allgens(A,d-1);
    Pie = allgens(A,e-1);
    Pif = allgens(A,f-1);
    L = flatten flatten flatten apply(
	Pid, U -> 
	   apply(Pie,V ->
	       apply(Pif, W -> (
bracket(A, {U,bracket(A,{V,W})}) == 
bracket(A, {bracket(A, {U,V}),W}) + (-1)^(d*e) * bracket(A, {V,bracket(A,{U,W})})
    ))))
)
L1 = flatten flatten flatten L
assert (all(L1, ell ->ell));
///




end--


uninstallPackage "HomotopyLieAlgebra"
restart
installPackage "HomotopyLieAlgebra"
check HomotopyLieAlgebra

viewHelp HomotopyLieAlgebra

