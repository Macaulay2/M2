-- -*- coding: utf-8 -*-
newPackage(
     "NumericalHilbert",
     PackageExports => {"NAGtypes"},
     Version => "0.1", 
     Date => "May 11, 2012",
     Authors => {{Name => "Robert Krone", 
    	       Email => "krone@math.gatech.edu"}},
     Headline => "some local Hilbert series functions",
     DebuggingMode => true
)

export {
     dualBasis,
     dualHilbert,
     standardBasis,
     gCorners,
     truncatedDual,
     zeroDimensionalDual,
     sCorners,
     localHilbertRegularity,
     orthogonalInSubspace,
     TruncDualData,
     truncDualData,
     nextTDD,
     homogPolySpace,
     newGCorners,
     origin,
     Seeds,
     DZ,
     ST,
     BM,
     GB,
     ProduceSB
     }

--TruncDualData private keys
protect Igens, protect syl, protect strategy, protect deg,
protect dBasis, protect hIgens, protect BMintegrals, protect BMcoefs

-----------------------------------------------------------------------------------------

-- Default tolerance value respectively for exact fields and inexact fields
defaultT := R -> if precision 1_R == infinity then 0 else 1e-6;
-- The origin in the coordinates of R
origin = method(TypicalValue=>Point)
origin Ring := R -> point matrix{toList(numgens R:0_R)};


truncatedDual = method(TypicalValue => DualSpace, Options => {Strategy => BM, Tolerance => null})
truncatedDual (Ideal,Point,ZZ) := o -> (I,p,d) -> truncatedDual(gens I,p,d,o)
truncatedDual (Matrix,Point,ZZ) := o -> (igens,p,d) -> (
    R := ring igens;
    t := if o.Tolerance === null then defaultT(R) else o.Tolerance;
    igens = sub(igens, matrix{gens R + apply(p.Coordinates,c->sub(c,R))});
    TDD := truncDualData(igens,false,t,Strategy=>o.Strategy);
    TDD = nextTDD(d,TDD,t);
    dualSpace(TDD,p)
    )

zeroDimensionalDual = method(TypicalValue => DualSpace, Options => {Strategy => BM, Tolerance => null})
zeroDimensionalDual (Ideal,Point) := o -> (I,p) -> zeroDimensionalDual(gens I,p,o)
zeroDimensionalDual (Matrix,Point) := o -> (igens,p) -> (
    R := ring igens;
    t := if o.Tolerance === null then defaultT(R) else o.Tolerance;
    igens = sub(igens, matrix{gens R + apply(p.Coordinates,c->sub(c,R))});
    TDD := truncDualData(igens,false,t,Strategy=>o.Strategy);
    dBasis := polySpace map(R^1,R^0,0);
    d := 0;
    while true do (
	dDim := dim dBasis;
    	TDD = nextTDD(d,TDD,t);
	dBasis = polySpace TDD;
	if dim dBasis == dDim then break;
	d = d+1;
	);
    dualSpace(dBasis,p)
    )

--An object that stores the data for an ongoing iterative tuncated dual space computation
TruncDualData = new Type of MutableHashTable
truncDualData = method(Options => {Strategy => BM, Seeds => null})
truncDualData (Matrix,Boolean,Number) := o -> (Igens,syl,t) -> (
    H := new MutableHashTable;
    R := ring Igens;
    H.Igens = Igens;
    H.syl = syl;
    H.strategy = o.Strategy;
    H.deg = 0;
    h := symbol h;
    S := (coefficientRing R)[{h}|gens R, MonomialOrder => {Weights => (numgens R+1):1, 1, (options R).MonomialOrder}]; --projectivization of R
    H.hIgens = homogenize(sub(Igens,S), h); 
    T := if syl then S else R;
    H.Seeds = if o.Seeds === null then dualSpace(matrix{{1_T}},origin(T)) else o.Seeds;
    H.BMmatrix = innerProduct(polySpace if syl then H.hIgens else H.Igens, H.Seeds);
    H.BMintegrals = gens H.Seeds;
    H.BMcoefs = numericalKernel(H.BMmatrix,t);
    H.dBasis = H.BMintegrals * H.BMcoefs;
    if H.syl then H.dBasis = (map(R,S,{1_R} | gens R)) H.dBasis;
    new TruncDualData from H
    )
-- advances the truncated dual computation from whatever was stored in parameters up to degree d
nextTDD = method()
nextTDD (TruncDualData,Number) := (H,t) -> nextTDD(H.deg + 1,H,t)
nextTDD (ZZ,TruncDualData,Number) := (d,H,t) -> (
    R := ring H.Igens;
    S := ring H.hIgens;
    if H.strategy == DZ then (
	Rd := polySpace basis(0,d,R);
	Id := DZspace(H.Igens,d,H.syl);
	H.dBasis = gens orthogonalInSubspace(Id,Rd,t);
	); 
    if H.strategy == BM then (
	dehomog := map(R,S,{1_R} | gens R);
     	for e from H.deg+1 to d do (
	    (M,E) := BMmatrix H;
	    H.BMmatrix = M; H.BMintegrals = E;
	    H.BMcoefs = numericalKernel(M,t);
	    H.dBasis = if H.syl then dehomog(E*H.BMcoefs) else H.dBasis | E*H.BMcoefs;
	    if numcols H.BMcoefs == 0 then break;
  	    );
    	);
    H.deg = d;
    H
    )
    
polySpace TruncDualData := o-> H -> polySpace(H.dBasis, Reduced=>false)
dualSpace (TruncDualData,Point) := (H,p) -> dualSpace(polySpace H,p)
homogPolySpace = method()
homogPolySpace TruncDualData := H -> polySpace(H.BMintegrals*H.BMcoefs) 

TEST ///
restart
R = CC[x,y]
M = matrix {{x^2-x*y^2,x^3}}
--M = matrix {{x*y, y^2}}
p = point matrix{{0_CC,0_CC}}
G = gCorners(ideal M,p,ProduceSB=>true)
G = gCorners(ideal M,p)
q = point matrix{{0_CC,1_CC}}
gCorners(M,q)
LDZ = reduceSpace truncatedDual(M,p,6,Strategy=>DZ)
LBM = reduceSpace truncatedDual(M,p,6,Strategy=>BM)
assert(areEqual(LDZ,LBM))
///

gCorners = method(TypicalValue => Sequence, Options => {Strategy => BM, Tolerance => null, ProduceSB => false})
gCorners (Ideal,Point) := o -> (I,p) -> gCorners(gens I,p,o)
gCorners (Matrix,Point) := o -> (igens,p) -> (
    R := ring igens;
    t := if o.Tolerance === null then defaultT(R) else o.Tolerance;
    igens = sub(igens, matrix{gens R + apply(p.Coordinates,c->sub(c,R))});

    ecart := max apply(flatten entries igens, g->(gDegree g - lDegree g)); --max ecart of generators
    GCs := {}; -- g-corners (as pairs: monomial, degree in homogenization)
    SBs := {}; -- standard basis elements (if o.ProduceSB)
    finalDegree := max(flatten entries igens / gDegree);
    d := 0;
    dBasis := dBasisReduced := polySpace map(R^1,R^0,0); -- Sylvester truncated dual space
    TDD := truncDualData(igens,true,t,Strategy=>o.Strategy); -- initial parameters for computing truncated duals
    while d <= finalDegree do (
    	TDD = nextTDD(d,TDD,t);
	dBasis = polySpace TDD;
	dBasisReduced = reduceSpace(dBasis,Tolerance=>t);
	-- Find new g-corners based on what monomials are missing from dBasis.
	newGCs := newGCorners(dBasisReduced,GCs,d,ecart);
	GCs = GCs|newGCs;
	-- If o.ProduceSB then compute a standard basis element for each new g-corner.
	if o.ProduceSB and #newGCs > 0 then SBs = SBs|newSBasis(dBasis,newGCs,d,t);
	-- Update stopping degree if there were new g-corners found.
	if #newGCs > 0 then (
	    topLCMdegree := max apply(subsets(#GCs,2),s->homogenizedLCMdegree(GCs#(s#0), GCs#(s#1)));
	    finalDegree = max(finalDegree,topLCMdegree);
	    );
	--print(d,(numrows M,numcols M), dim Rd, newGCs/first);
	d = d+1;
	);
    GCs = if o.ProduceSB then SBs else GCs/first;
    (dBasisReduced, sbReduce matrix {GCs})
    )

-- computes s-corners from the g-corners
-- i.e. the maximal monomials not in the ideal generated by the g-corners
sCorners = method(TypicalValue => Matrix)
sCorners MonomialIdeal := I -> sCorners gens I
sCorners Matrix := gCs -> (
    R := ring gCs;
    n := numgens R;
    G := flatten entries gCs;
    candidates := subsets(G, n) / listLCM;
    S := select(candidates, c -> (
	    c != 0 and
	    all(G, g -> not isDivisible(c,g)) and
	    all(gens R, v -> any(G, g -> isDivisible(v*c,g)))));
    matrix{S}
    )


localHilbertRegularity = method(TypicalValue => ZZ, Options=>{Tolerance => null})
localHilbertRegularity (Ideal, Point) := o -> (I,p) -> localHilbertRegularity(gens I,p,o)
localHilbertRegularity (Matrix, Point) := o -> (Igens,p) -> (
    n := numgens ring Igens;
    gCs := last gCorners(Igens,p,o);
    gCLists := apply(flatten entries gCs, l -> (listForm l)#0#0);
    LCMexp := apply(n, i -> max(apply(gCLists, l->l#i)));
    max{sum LCMexp - n + 1, 0}
    )
    

TEST ///
restart
R = CC[x,y]
G1 = matrix{{x^2,x*y^2,y^4}}
assert(sCorners G1 == matrix {{x*y, y^3}})
G2 = matrix{{x*y^2,x^2*y^2,y^4}}
assert(sCorners G2 == matrix {{y^3}})
G3 = matrix{{x*y^2,x^2*y^2}}
assert(sCorners G3 == matrix {{}})
///

listLCM = L -> (
    R := ring L#0;
    L = apply(L, l -> (listForm l)#0#0);
    LCMexp := apply(numgens R, i -> max(apply(L, l->l#i)));
    LCMexp = LCMexp - toList ((numgens R):1);
    if not all(LCMexp, a -> a >= 0) then return 0;
    product(numgens R, i -> R_i^(LCMexp#i))
    )    

orthogonalInSubspace = method()
orthogonalInSubspace (DualSpace, PolySpace, Number) := (D,S,t) -> (
    M := innerProduct(S,D);
    K := numericalKernel(transpose M,t);
    polySpace((gens S)*K, Reduced=>false)
    )
orthogonalInSubspace (PolySpace, PolySpace, Number) := (T,S,t) -> (
    T' := dualSpace(T, origin(ring S));
    orthogonalInSubspace(T',S,t)
    )

-- Implementation of algorithm from 1996 paper of Bernard Mourrain.
-- M is the main matrix
-- E is the row matrix of all dual basis integrals (including 1)
-- B contains the most recently found generators (as coefficients in terms of E)
BMmatrix = H -> (
    Igens := if H.syl then H.hIgens else H.Igens;
    (M,E,B,homogeneous,Seeds) := (H.BMmatrix,H.BMintegrals,H.BMcoefs,H.syl,H.Seeds);
    --print(Igens,M,E);
    R := ring Igens;
    n := numgens R;
    m := numcols Igens;
    snew := numcols B;
    offset := if homogeneous then 0 else dim Seeds;
    s := (numcols E - offset)//n; --number of dual space generators
    npairs := subsets(n,2);
    newMEs := apply(snew, i -> (
	    bcol := B_{i};
	    bpoly := (E*bcol)_(0,0);
	    E' := matrix {apply(n, k->(
			subs := matrix{apply(n, l->(if l > k then 0_R else (gens R)#l))};
			(gens R)#k * sub(bpoly,subs)))};
	    M' := innerProduct(polySpace Igens, polySpace E');
	    if not homogeneous then M' = map(R^(s+snew),R^n,0) || M';
	    for j from 0 to s-1 do (
		w := apply(n,k->(bcol_(offset + j*n + k,0)));
		v := mutableMatrix(R,#npairs,n);
		for i from 0 to #npairs-1 do (
		    v_(i,npairs#i#0) =  w#(npairs#i#1);
		    v_(i,npairs#i#1) = -w#(npairs#i#0);
		    );
		M' = M' || new Matrix from v;
		);
	    (M',E')
	    ));
    
    if not homogeneous then
    M = transpose B || M || map(R^(m + s*(1+#npairs) - numrows M),R^(numcols E),0)
    else M = map(R^(m + s*#npairs),R^0,0);
    M = M | matrix{newMEs/first};
    E = if homogeneous then matrix{newMEs/last} else E | matrix{newMEs/last};
    (M,E)
    );


newGCorners = method()
newGCorners (PolySpace,List,ZZ,ZZ) := (dBasis,GCs,d,ecart) -> (
    R := ring dBasis;
    mons := flatten entries sort basis(d-ecart,d,R);
    dBasisMons := sort(flatten entries gens dBasis / gLeadMonomial);
    newGCs := {};
    i := 0;
    for m in mons do (
	while i < #dBasisMons and m > dBasisMons#i do i = i+1;
	if i < #dBasisMons and m == dBasisMons#i then (i = i+1; continue);
	if not any(GCs, g->(isDivisible(m,g#0) and gDegree m - gDegree(g#0) <= d - g#1)) then newGCs = append(newGCs, (m,d));
	);
    newGCs
    )

newSBasis = (dBasis,newGCs,d,t) -> (
    R := ring dBasis;
    Rd := sort basis(0,d,R);
    Id := orthogonalInSubspace(dBasis, polySpace Rd, t);
    iBasis := flatten entries gens reduceSpace(Id,Monomials=>Rd);
    new List from apply(newGCs, n->first select(1,iBasis, b->(lLeadMonomial b == n#0)))
    )

homogenizedLCMdegree = (a,b) -> (
     alist := ((listForm(a#0))#0#0);
     blist := ((listForm(b#0))#0#0);
     lcmlist := apply(alist,blist, (i,j)->max(i,j));
     tdegree := max(a#1 - sum alist, b#1 - sum blist);
     sum lcmlist + tdegree
     )

--Dayton-Zeng matrix to find the the dual space up to degree d.
DZspace = method(TypicalValue => Matrix)
DZspace (Matrix, ZZ, Boolean) := (igens, d, syl) -> (
     R := ring igens;
     igens = first entries igens;
     genDeg := if syl then gDegree else lDegree;
     p := map(R^1,R^0,0);
     for g in igens do
	 p = p|(matrix{{g}}*basis(0, d - genDeg g, R));
     polySpace p
     )

--checks each monomial of degree d and counts ones in the monomial basis of the quotient space
hilbertB = method(TypicalValue => ZZ)
hilbertB (List, ZZ) := (sbElements, d) -> (
     R := ring first sbElements;
     G := sbElements / leadMonomial;
     #select(first entries basis(d,R), m->(#select(G, g->isDivisible(m,g)) == 0))
     )

--finds Hilbert series values combinatorially using inclusion-exclusion
hilbertC = method(TypicalValue => ZZ)
hilbertC (List, ZZ) := (sbElements, d) -> (
     R := ring first sbElements;
     n := #gens R;
     sbListForm := apply(sbElements, e -> (listForm e)#0#0); --store lead monomials as n-tuples of integers
     listFormLcm := L -> apply(n, i->max{max(apply(L,l->l#i)), 0});
     coef := s -> if even(#s) then 1 else -1;
     hsum := 0;
     for s in subsets sbListForm do
	  hsum = hsum + (coef s)*bin(d - sum listFormLcm s + n-1, n-1);
     hsum
     )

--returns if lead term of a is divisible by lead term of b
isDivisible = (a, b) -> (
     dif := (listForm a)#0#0 - (listForm b)#0#0;
     all(dif, i->(i >= 0))
     )

--binomial coefficient 
bin = (m,k) -> if m >= 0 then binomial(m,k) else 0

--lead monomial and lead monomial degree according to ordering associated with
--the ring (local) and reverse ordering (global)
lLeadMonomial = f -> leadMonomial last terms f
gLeadMonomial = f -> leadMonomial first terms f
lDegree = f -> first degree lLeadMonomial f
gDegree = f -> first degree gLeadMonomial f

--remove non-minimal standard basis elements
sbReduce = L -> (
    n:= numcols L;
    goodi := select(n, i->(
     	    all(0..n-1, j->(j == i or not isDivisible(lLeadMonomial L_(0,i), lLeadMonomial L_(0,j))))
	    ));
    L_goodi
    )

{*
beginDocumentation()

doc ///
     Key
     	  NumericalHilbert
     Headline
     	  functions for numerically computing local dual space and Hilbert functions
     Description
     	  Text
	       @EM "NumericalHilbert"@ gets stuff done.
///

doc ///
     Key
          DualSpace
     Description
          Text
	        a Type.
///

doc ///
     Key
          dualHilbert
     Headline
          Calculate Hilbert series of the dual of a polynomial ideal
     Usage
          dualHilbert(gns, d)
     Inputs
          gns:Matrix
	       generators of an ideal in a one-row matrix
	  d:ZZ
     Outputs
          :List
     Description
          Text
	       Calculates dimension of the dual space of an ideal at each degree from 0 to d inclusive.
	  Example
///

doc ///
     Key
          dualBasis
     Headline
          Calculate basis of the dual space of a polynomial ideal
     Usage
          dualBasis(gns, d)
     Inputs
          gns:Matrix
	       generators of an ideal in a one-row matrix
	  d:ZZ
     Outputs
          :dualSpace
	       basis of the dual space in a one-row matrix
     Description
          Text
	       Calculates a reduced basis of the truncated dual space of an ideal.  It's truncated at degree d.
	       Elements are expressed as elements of the polynomial ring of the ideal although this is an abuse of notation.
	       They are really elements of the dual ring.
	  Example
///

doc ///
     Key
          standardBasis
     Headline
          Calculate the standard basis of a polynomial ideal numerically
     Usage
          standardBasis(gns)
     Inputs
          gns:Matrix
	       generators of an ideal in a one-row matrix
	  d:ZZ
     Outputs
          :Matrix
     Description
          Text
	       Finds a standard basis of the ideal using the dual basis calculation, which is numerically stable.
	  Example
///
*}
end


restart
--loadPackage "NumericalHilbert"
R = CC[x,y]
M = matrix {{x^2-x*y^2,x^3}}
--M = matrix {{x*y}}
--M = matrix {{x^9 - y}}
p = point matrix{{0_CC,0_CC}}
q = point matrix{{1_CC,0_CC}}
L = reduceSpace truncatedDual(M,p,6,Strategy=>DZ)
L = reduceSpace truncatedDual(M,p,6,Strategy=>BM)
--shiftDual(L,q,8)
G = matrix{{x^2,x*y^2,y^4}}
sCorners G

hilbertFunction(toList(0..8),L)
dualInfo(M,p,Truncate=>8)
standardBasis(M)
dualHilbert(M,Truncate=>25)
dualBasis(M)
dualInfo(M)
dualInfo(M, Strategy=>DZ)
dualInfo(M,Point=>{0.01,0.01})

-- small example
restart
loadPackage ("NumericalHilbert", Reload => true)
R = QQ[x,y,z, MonomialOrder => {Weights=>{-1,-1,-1}}, Global => false]
M = matrix {{x-y^2, y-z^2}}
RH = QQ[t,x,y,z,MonomialOrder => {Weights=>{-1,-1,-1,-1}}, Global => false]
MH = homogenize(sub(M,RH),t)	  
DH = flatten entries gens dualBasis(MH,4)
DHhat = sub(matrix{select(DH, Q->first first last listForm Q >= 1)}, {t=>1})
(
     sort flatten entries gens dualBasis(M,3)
     ) == (
     sort unique flatten entries sub(DHhat,R)
     )
