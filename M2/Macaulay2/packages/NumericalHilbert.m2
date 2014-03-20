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
     zerDimensionalDual,
     sCorners,
     orthogonalInSubspace,
     initialTDParameters,
     nextTD,
     newGCorners,
     origin,
     DZ,
     ST,
     BM,
     GB,
     ProduceSB
     }

-----------------------------------------------------------------------------------------

-- Default tolerance value respectively for exact fields and inexact fields
defaultT := R -> if precision 1_R == infinity then 0 else 1e-6;
-- The origin in the coordinates of R
origin = method(TypicalValue=>Point)
origin Ring := R -> point matrix{toList(numgens R:0_R)};

{*
dualBasis = method(TypicalValue => DualSpace, Options => {Truncate => -1, Point => {}, Strategy => BM, Tolerance => -1.})
dualBasis (Matrix) := o -> (igens) -> (dualInfo(igens, Truncate=>o.Truncate, Point=>o.Point, Strategy=>o.Strategy, Tolerance=>o.Tolerance))#0;

standardBasis = method(TypicalValue => Matrix, Options => {Truncate => -1, Point => {}, Strategy => BM, Tolerance => -1.}) 
standardBasis (Matrix) := o -> (igens) -> (dualInfo(igens, Truncate=>o.Truncate, Point=>o.Point, Strategy=>o.Strategy, Tolerance=>o.Tolerance, ProduceSB=>true))#2;

dualHilbert = method(TypicalValue => List, Options => {Truncate => -1, Point => {}, Strategy => BM, Tolerance => -1.})
dualHilbert (Matrix) := o -> (igens) -> (dualInfo(igens, Truncate=>o.Truncate, Point=>o.Point, Strategy=>o.Strategy, Tolerance=>o.Tolerance))#4;


shiftDual = method(TypicalValue => DualSpace)
shiftDual (DualSpace,Point,ZZ) := (L,p,d) -> (
    q := coordinates p - coordinates L.BasePoint;
    shiftSeqs := apply(gens ring L, q, (v,c)->sum(d+1,i->(c*v)^i));
    subs := apply(gens ring L, shiftSeqs, (v,s)->(v => v*s));
    newBasis := sub(gens L, subs);
    newBasis = product(shiftSeqs)*newBasis;
    (mons,coefs) := coefficients(newBasis, Monomials => basis(0,d,ring L));
    newBasis = mons*coefs;
    dualSpace(polySpace newBasis,p)
    )
*}

truncatedDual = method(TypicalValue => DualSpace, Options => {Strategy => BM, Tolerance => -1.})
truncatedDual (Ideal,Point,ZZ) := o -> (I,p,d) -> truncatedDual(gens I,p,d,o)
truncatedDual (Matrix,Point,ZZ) := o -> (igens,p,d) -> (
    R := ring igens;
    t := if o.Tolerance == -1. then defaultT(R) else o.Tolerance;
    igens = sub(igens, matrix{gens R + apply(p.Coordinates,c->sub(c,R))});
    parameters := initialTDParameters(igens,false,Strategy=>o.Strategy);
    dBasis := polySpace map(R^1,R^0,0);
    (dBasis, parameters) = nextTD(d,parameters,t);
    dualSpace(dBasis,p)
    )

zeroDimensionalDual = method()
zeroDimensionalDual (Ideal,Point) := o -> (I,p) -> zeroDimensionalDual(gens I,p,o)
zeroDimensionalDual (Matrix,Point) := o -> (igens,p) -> (
    R := ring igens;
    t := if o.Tolerance == -1. then defaultT(R) else o.Tolerance;
    igens = sub(igens, matrix{gens R + apply(p.Coordinates,c->sub(c,R))});
    parameters := initialTDParameters(igens,false,Strategy=>o.Strategy);
    dBasis := polySpace map(R^1,R^0,0);
    d := 0;
    while true do (
	dDim := dim dBasis;
    	(dBasis, parameters) = nextTD(d,parameters,t);
	if dim dBasis == dDim then break;
	d = d+1;
	);
    dualSpace(dBasis,p)
    )

-- initialize iterated truncated dual values
--  igens: ideal generators
--  syl: boolean specifying whether to compute Sylvester dual, or normal dual space
--  strategy: either BM or DZ
initialTDParameters = method(Options => {Strategy => BM})
initialTDParameters (Matrix,Boolean) := o -> (igens,syl) -> (
    R := ring igens;
    h := symbol h;
    S := (coefficientRing R)[{h}|gens R, MonomialOrder => {Weights => (numgens R+1):1, 1, (options R).MonomialOrder}]; --projectivization of R
    higens := homogenize(sub(igens,S), h);
    MEB := if syl then 3:map(S^1,S^0,0) else 3:map(R^1,R^0,0);
    (igens,higens,syl,o.Strategy,-1,map(R^1,R^0,0))|MEB
    )
-- advances the truncated dual computation from whatever was stored in parameters up to degree d
nextTD = method()
nextTD (ZZ,Sequence,Number) := (d,parameters,t) -> (
    (igens,higens,syl,strategy,dStart,dBasis,M,E,B) := parameters;
    R := ring igens;
    S := ring higens;
    if strategy == DZ then (
	Rd := polySpace basis(0,d,R);
	Id := DZspace(igens,d,syl);
	dBasis = gens orthogonalInSubspace(Id,Rd,t);
	); 
    if strategy == BM then (
	dehomog := map(R,S,{1_R} | gens R);
     	for e from dStart+1 to d do (
	    (M,E,B) = BMmatrix(if syl then higens else igens,M,E,B,t,syl);
	    dBasis = if syl then dehomog(E*B) else dBasis | E*B;
	    if numcols B == 0 then break;
  	    );
    	);
    (polySpace(dBasis,Reduced=>false), (igens,higens,syl,strategy,d,dBasis,M,E,B))
    )
    

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
reduceSpace truncatedDual(M,q,6,Strategy=>BM)
assert(areEqual(LDZ,LBM))
///

gCorners = method(TypicalValue => Sequence, Options => {Strategy => BM, Tolerance => -1., ProduceSB => false})
gCorners (Ideal,Point) := o -> (I,p) -> gCorners(gens I,p,o)
gCorners (Matrix,Point) := o -> (igens,p) -> (
    R := ring igens;
    t := if o.Tolerance == -1. then defaultT(R) else o.Tolerance;
    igens = sub(igens, matrix{gens R + apply(p.Coordinates,c->sub(c,R))});

    ecart := max apply(flatten entries igens, g->(gDegree g - lDegree g)); --max ecart of generators
    GCs := {}; -- g-corners (as pairs: monomial, degree in homogenization)
    SBs := {}; -- standard basis elements (if o.ProduceSB)
    finalDegree := max(flatten entries igens / gDegree);
    d := 0;
    dBasis := dBasisReduced := polySpace map(R^1,R^0,0); -- Sylvester truncated dual space
    parameters := initialTDParameters(igens,true,Strategy=>o.Strategy); -- initial parameters for computing truncated duals
    while d <= finalDegree do (
    	(dBasis,parameters) = nextTD(d,parameters,t);
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

{*
eliminatingDual = method(TypicalValue => List, Options => {Point => {}, Tolerance => -1.})
eliminatingDual (Matrix, ZZ, List) := o -> (igens, r, varList) -> (
     R := ring igens;
     n := numgens R;
     tol := o.Tolerance;
     if tol == -1. then (if precision 1_R == infinity then tol = 0. else tol = defaultT());
     if o.Point != {} then igens = sub(igens, matrix{gens R + apply(o.Point,p->sub(p,R))});
     lastd := -1;
     d := 0;
     dmons := {};
     eBasis := {};
     eBasisSize := 0;
     ecart := max apply(first entries igens, g->(gDegree g - lDegree g));
     wvec := new MutableList from (n:0);
     for v in varList do apply(n, i->(if v == R_i then wvec#i = -1));
     wvec = toList wvec;
     S := (coefficientRing R)(monoid[gens R, MonomialOrder => {Weights=>wvec,Weights=>n:-1}, Global => false]);
     while d <= lastd + ecart + 1 do (
	  dmons = dmons | entries basis(d,R);
	  M := transpose DZmatrix(igens,d,false);
	  kern := findKernel(M,tol);
	  dualBasis := (matrix{flatten dmons})*sub(kern,R);
	  dualBasis = sub(dualBasis,S);
	  (mons, N) := coefficients dualBasis;
	  dualBasis = flatten entries parseKernel(sub(N,coefficientRing S),entries mons,tol);
	  eBasis = select(dualBasis, b->(degree(S_0, last terms b) <= r));
	  if #eBasis > eBasisSize then lastd = d;
	  eBasisSize = #eBasis;
	  d = d+1;
	  );
     apply(eBasis, b->sub(b,R))
     );
*}


-- Implementation of algorithm from 1996 paper of Bernard Mourrain.
-- M is the main matrix
-- E is the row matrix of all dual basis integrals (including 1)
-- B contains the most recently found generators (as coefficients in terms of E)
BMmatrix = (igens, M, E, B, tol, homogeneous) -> (
    --print(igens,M,E,bpairs);
    R := ring igens;
    n := numgens R;
    m := numcols igens;
    snew := numcols B;
    offset := if homogeneous then 0 else 1;
    s := (numcols E - offset)//n; --number of dual space generators
    npairs := subsets(n,2);
    if snew == 0 then ( -- degree 0
	M = transpose sub(igens,map(R^1,R^n,0));
	return (M, matrix{{1_R}}, numericalKernel(M,tol));
	);
    newMEs := apply(snew, i -> (
	    bcol := B_{i};
	    bpoly := (E*bcol)_(0,0);
	    E' := matrix {apply(n, k->(
			subs := matrix{apply(n, l->(if l > k then 0_R else (gens R)#l))};
			(gens R)#k * sub(bpoly,subs)))};
	    M' := innerProduct(polySpace igens, polySpace E');
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
    B = numericalKernel(M, tol);
    (M, E, B)
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
    Rd := rsort basis(0,d,R);
    Id := orthogonalInSubspace(dBasis, polySpace Rd, t);
    M := (coefficients(gens Id, Monomials=>Rd))#1;
    iBasis := flatten entries (Rd*colReduce(M,t));
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
