-- -*- coding: utf-8 -*-
{*
newPackage(
     "PolynomialSpace",
     PackageExports => {"NAGtypes"},
     Version => "0.1", 
     Date => "March 29, 2014",
     Authors => {
	 {Name => "Robert Krone", Email => "krone@math.gatech.edu"},
	 {Name => "Anton Leykin", Email => "leykin@math.gatech.edu"}
	 },
     Headline => "functionality for PolySpace and DualSpace types and numerical linear algebra",
     DebuggingMode => true
)
*}
export {
     "addition", "intersection", "reduceSpace",
     "colon", "innerProduct", "isContained",
     "numericalKernel", "numericalImage", "colReduce"
     }

------------------------------------------------

hilbertFunction DualSpace := L -> (
    if not L.Space.Reduced then L = reduceSpace L;
    tally(flatten entries gens L / first @@ degree)
    )
hilbertFunction (List,DualSpace) := (LL,L) -> (
    h := hilbertFunction L;
    apply(LL, d->(if h#?d then h#d else 0))
    )
hilbertFunction (ZZ,DualSpace) := (d,L) -> first hilbertFunction({d},L)

check DualSpace :=  L -> error "not implemented"

areEqual (PolySpace,PolySpace) := o -> (S,T) -> (
    n := dim addition(S,T,Tolerance=>o.Tolerance);
    n == dim S and n == dim T
    )
areEqual (DualSpace,DualSpace) := o -> (L,K) ->
    areEqual(L.BasePoint,K.BasePoint,Tolerance=>o.Tolerance) and areEqual(L.Space,K.Space,Tolerance=>o.Tolerance)
    
isContained = method(TypicalValue => Boolean, Options => {Tolerance=>1e-6})
isContained (PolySpace,PolySpace) := o -> (S,T) ->
    dim addition(S,T,Tolerance=>o.Tolerance) == dim T
isContained (DualSpace,DualSpace) := o -> (L,K) ->
    areEqual(L.BasePoint,K.BasePoint,Tolerance=>o.Tolerance) and isContained(L.Space,K.Space,Tolerance=>o.Tolerance)

intersection = method(TypicalValue => PolySpace, Options => {Tolerance=>1e-6})
intersection (PolySpace,PolySpace) := o -> (S,T) -> (
    (mons,coefs) := coefficients (gens S|gens T);
    Scoefs := submatrix(coefs,(0..dim S-1));
    Tcoefs := submatrix'(coefs,(0..dim T-1));
    Sorth := numericalKernel(transpose Scoefs,o.Tolerance);
    Torth := numericalKernel(transpose Tcoefs,o.Tolerance);
    M := mons*numericalKernel(transpose (Sorth|Torth),o.Tolerance);
    polySpace M
    )

addition = method(TypicalValue => PolySpace, Options => {Tolerance=>1e-6})
addition (PolySpace,PolySpace) := o -> (S,T) -> (
    (mons,C) := coefficients (gens S | gens T);
    polySpace(mons*sub(numericalImage(C,o.Tolerance),ring S))
    )

reduceSpace = method(Options => {Monomials => null,Tolerance=>1e-6})
reduceSpace PolySpace := o -> S -> (
    if dim S == 0 then return polySpace(gens S,Reduced=>true);
    (mons,coefs) := coefficients(gens S, Monomials => o.Monomials);
    M := mons*(colReduce(coefs,o.Tolerance));
    polySpace(M,Reduced=>true)
    )
reduceSpace DualSpace := o -> L -> dualSpace(reduceSpace L.Space,L.BasePoint)

colon = method(TypicalValue => DualSpace, Options => {Tolerance=>1e-6})
colon (DualSpace, RingElement) := o-> (L,g) -> (
    (gmons,gcoefs) := coefficients g;
    (Lmons,Lcoefs) := coefficients gens L;
    M := matrix apply(flatten entries gmons, gm->(
	    apply(flatten entries Lmons, Lm->(
		    d := diff(gm,Lm);
		    if d == 0 then d else leadMonomial d
		    ))
	    ));
    if numcols M == 0 then M = map((ring L)^1,(ring L)^0,0);
    M = (transpose gcoefs)*M*Lcoefs;
    (Mmons,Mcoefs) := coefficients M;
    M = Mmons*sub(numericalImage(Mcoefs,o.Tolerance),ring Mmons);
    dualSpace(polySpace M, L.BasePoint)
    )
colon (DualSpace, Ideal) := (L,J) -> error "not implemented"

-- Matrix of inner products
-- PolySpace generators as rows, DualSpace generators as columns
innerProduct = method()
innerProduct (PolySpace, PolySpace) := (S, T) -> (
    M := last coefficients(gens S | gens T);
    Svec := submatrix(M,0..dim S-1);
    Tvec := submatrix'(M,0..dim S-1);
    (transpose Svec)*Tvec
    )
innerProduct (PolySpace, DualSpace) := (S, L) -> (
    Sshift := polySpace sub(gens S, matrix{(gens ring L) + coordinates L.BasePoint});
    innerProduct(Sshift, L.Space)
    )
innerProduct (RingElement, DualSpace) := (f, L) -> innerProduct(polySpace matrix{{f}}, L)
innerProduct (RingElement, RingElement) := (f, l) -> (
    M := last coefficients(matrix{{f,l}});
    ((transpose M_{0})*M_{1})_(0,0)
    )

random PolySpace := o -> S -> (
    F := ultimate(coefficientRing, ring S);
    ((gens S)*sub(random(F^(dim S),F^1), ring S))_(0,0)
    )
random (ZZ,PolySpace) := o -> (d,S) -> (
    if not S.Reduced then S = reduceSpace S;
    Sd := polySpace sub(matrix{select(flatten entries gens S, q -> first degree q <= d)}, ring S);
    random Sd
    )
random DualSpace := o -> D -> random D.Space
random (ZZ,DualSpace) := o -> (d,D) -> random(d,D.Space)

interpolatedIdeal = method()
interpolatedIdeal DualSpace := L -> error "not implemented"
interpolatedIdeal List := LL -> error "not implemented"

---------------------------------------------
-- Numerical Linear Algebra
---------------------------------------------

numericalImage = method()
numericalImage (Matrix, Number) := (M, tol) -> (
    R := ultimate(coefficientRing, ring M);
    M = sub(M, R);
    if numcols M == 0 then return M;
    if numrows M == 0 then return map(R^0,R^0,0);
    if precision 1_(ring M) < infinity then (
	(svs, U, Vt) := SVD M;
	cols := positions(svs, sv->(sv > tol));
	submatrix(U,,cols)
	) else (
	gens image M
	)
    )

numericalKernel = method()
numericalKernel (Matrix, Number) := (M, tol) -> (
    R := ring M;
    M = sub(M, ultimate(coefficientRing, R));
    if numrows M == 0 then return id_(source M);
    if numcols M == 0 then return map(R^0,R^0,0);
    if precision 1_R < infinity then (
	(svs, U, Vt) := SVD M;
	cols := positions(svs, sv->(sv > tol));
	submatrix'(adjointMatrix Vt,,cols)
	) else (
	gens kernel M
	)
    )

-- produces the conjugate transpose
adjointMatrix = method(TypicalValue => Matrix)
adjointMatrix Matrix := M -> (
    M' := mutableMatrix transpose M;
    for i from 0 to (numrows M')-1 do (
	for j from 0 to (numcols M')-1 do M'_(i,j) = conjugate(M'_(i,j));
	);
    matrix M'
    )

--performs Gaussian reduction on M
colReduce = method(TypicalValue => Matrix)
colReduce (Matrix, Number) := (M, tol) -> (
    M = new MutableMatrix from sub(transpose M, ultimate(coefficientRing, ring M));
    (m,n) := (numrows M, numcols M);
    i := 0; --row of pivot
    for j from 0 to n-1 do (
	if i == m then break;
	a := i + maxPosition apply(i..m-1, l->(abs M_(l,j)));
	c := M_(a,j);
	if abs c <= tol then continue;
	rowSwap(M,a,i);
	for l from 0 to n-1 do M_(i,l) = M_(i,l)/c; --rowMult(M,i,1/c); is bugged
	for k from 0 to m-1 do rowAdd(M,k,-M_(k,j),i);
	i = i+1;
	);
    M = (transpose new Matrix from M)_{0..i-1};
    if tol > 0 then clean(tol,M) else M
    )