------------------------------------------------------
-- deflation and numerical rank
-- (loaded by  ../NumericalAlgebraicGeometry.m2)
------------------------------------------------------

export { "deflate", 
    "SolutionSystem", "Deflation", "DeflationRandomMatrix", "liftPointToDeflation", 
    "deflateInPlace", "DeflationSequence", "DeflationSequenceMatrices",
    "LiftedPoint", "LiftedSystem", "SquareUp",
    "numericalRank", "isFullNumericalRank"
    }

numericalRank = method(Options=>{Threshold=>1e-4})
numericalRank Matrix := o -> M -> (
     o = fillInDefaultOptions o;
     if not member(class ring M, {RealField,ComplexField}) 
     then error "matrix with real or complex entries expected";
     t := o.Threshold; 
     N := if t<=1 then M -- use t as an absolute cutoff for singular values, otherwise look for a "gap"  
          else matrix apply(entries M, row->(      -- nomalize "large" rows (a hack!!!)
	     	  m := max(row/abs);
	     	  if m<1 then row else apply(row,e->e/m)
	     	  ));
     S := first SVD N;
     r := 0; last's := 1;
     for i to #S-1 do (
	 if t>1 then (if t*S#i < last's 
	     then break
	     else (r = r + 1; last's = S#i)
	     )
	 else (
	     if S#i>t then r = r + 1
	     else break 
	     )
	 );
     r 
     )  

isFullNumericalRank = method(Options=>{Threshold=>1e-4}) 
isFullNumericalRank Matrix := o -> M -> (
    r := numericalRank(M,o);
    r == min(numColumns M, numRows M) 
    )

TEST ///
C=CC_200
C[x,y,z]
F = polySystem {x^3,y^3,x^2*y,z*(z^2-1)^2}
e = 0.00001
P0 = point{{e,e,ii*e}}
P1 = point{{e,e,e+ii*e}}
P2 = point{{0.1,0.1,0.1_CC}}
assert not isFullNumericalRank evaluate(F,P0)
assert not isFullNumericalRank evaluate(F,P1)
assert isFullNumericalRank evaluate(F,P2)
///

deflate = method(Options=>{Variable=>null})

-- creates and stores (if not stored already) a deflated system and the corresponding random matrix 
-- returns the deflation rank
deflate (PolySystem, Point) := o -> (F,P) -> (
    J := evaluate(jacobian F, P);
    r := numericalRank J;
    deflate(F,r,o);
    r
    )	

-- creates and stores (if not stored already) 
-- returns a deflated system for rank r
deflate (PolySystem, ZZ) := o -> (F,r) -> (
    if not F.?Deflation then (
	F.Deflation = new MutableHashTable;
	F.DeflationRandomMatrix = new MutableHashTable;
	);
    if not F.Deflation#?r then (
	C := coefficientRing ring F;
	B := random(C^(F.NumberOfVariables),C^(r+1));
    	deflate(F,B,o);
	); 
    F.Deflation#r
    )

-- deflate using a matrix 
deflate(PolySystem, Matrix) := o -> (F,B) -> (
    if not F.?Deflation then (
	F.Deflation = new MutableHashTable;
	F.DeflationRandomMatrix = new MutableHashTable;
	);
    r := numcols B - 1;
    L := if o.Variable === null then symbol L else o.Variable;
    C := coefficientRing ring F;
    R := C (monoid [gens ring F, L_1..L_r]);
    LL := transpose matrix{ take(gens R, -r) | {1_C} };
    RFtoR := map(R, ring F);
    F.Deflation#r = polySystem (RFtoR F.PolyMap || (RFtoR jacobian F)*B*LL);
    F.DeflationRandomMatrix#r = B; 
    F.Deflation#r
    )

-- deflate using a pair of matrices: one for deflation, the other for squaring up 
deflate(PolySystem, Sequence) := o -> (F,BM) -> (
    (B,M) := BM;
    FD := deflate(F,B,o); -- passing non-null option may not work!!!
    squareUp(FD,M)
    )

-- deflate according to the sequence of matrices (or pairs of matrices if squaring up)
deflate(PolySystem, List) := o -> (F, seq) -> (
    scan(seq, B -> F = deflate(F,B,o)); -- here B is either a Matrix or (Matrix,Matrix)
    F
    )

-- deflation ideal
deflate Ideal := o -> I -> (
    C := coefficientRing ring I;
    F := polySystem transpose gens I;
    B := map C^(F.NumberOfVariables) | map(C^(F.NumberOfVariables),C^1,0);
    ideal deflate(F,B,o)
    )

liftPointToDeflation = method() 
-- approximates the coordinates corresponding to the augmented deflation variables
-- for the deflation of F of rank r
liftPointToDeflation (Point,PolySystem,ZZ) := (P,F,r) -> (
    if F.NumberOfVariables == (F.Deflation#r).NumberOfVariables then P
    else (
    	A := evaluate(jacobian F, P)*(F.DeflationRandomMatrix#r);
    	last'column := numColumns A-1; 
    	point {
	    coordinates P | 
	    flatten entries solve(submatrix'(A,{last'column}),-submatrix(A,{last'column}),ClosestFit=>true)
	    }
	) 
    )
   
TEST ///
C=CC_200
C[x,y,z]
F = polySystem {x^3,y^3,x^2*y,z^2}
P0 = point sub(matrix{{0.000001, 0.000001*ii,0.000001-0.000001*ii}},C)
assert not isFullNumericalRank evaluate(jacobian F,P0)
r1 = deflate (F,P0)
P1' = liftPointToDeflation(P0,F,r1) 
F1 = F.Deflation#r1
P1 = newton(F1,P1')
assert not isFullNumericalRank evaluate(jacobian F1,P1)
r2 = deflate (F1,P1)
P2' = liftPointToDeflation(P1,F1,r2) 
F2 = F1.Deflation#r2
P2 = newton(F2,P2')
assert isFullNumericalRank evaluate(jacobian F2,P2)
P = point {take(coordinates P2, F.NumberOfVariables)}
assert(residual(F,P) < 1e50)
NP2 = newton(F2,P2)
NNP2 = newton(F2,NP2)
NP2.ErrorBoundEstimate
NNP2.ErrorBoundEstimate
assert(P2.ErrorBoundEstimate^2 > NP2.ErrorBoundEstimate)
///

deflateInPlace = method(Options=>{SquareUp=>true})
deflateInPlace(Point,PolySystem) := o -> (P,F) -> (
    P0 := P;
    F0 := F;
    d'seq := {}; -- deflation sequence: a sequence of matrices used for deflation
    d'seq'mat := {}; -- ... corresponding matrices      
    assert isSolution(P,F);
    if (status P =!= Regular or (not P.?SolutionSystem) or P.SolutionSystem =!= F) then
    while not isFullNumericalRank evaluate(jacobian F0,P0) do (
	r := deflate (F0,P0);
	d'seq = d'seq | {r};
	new'mat'or'pair := F0.DeflationRandomMatrix#r;
	P0' := liftPointToDeflation(P0,F0,r); 
	F0 = F0.Deflation#r; 
	if o.SquareUp then (
	    F0' := squareUp F0;
	    new'mat'or'pair = (new'mat'or'pair, squareUpMatrix F0);
	    F0 = F0';
	    );
	d'seq'mat = d'seq'mat | {new'mat'or'pair}; 
	P0 = newton(F0,P0');
	);
    P.Coordinates = take(coordinates P0, F.NumberOfVariables);
    if #d'seq>0 then P.ErrorBoundEstimate = P0.ErrorBoundEstimate;
    P.DeflationSequence = d'seq;
    P.DeflationSequenceMatrices = d'seq'mat;
    P.SolutionSystem = F;
    P.LiftedSystem = P0.SolutionSystem = F0;
    P.LiftedPoint = P0;
    P.SolutionStatus = if #d'seq > 0 then Singular else Regular
    )

TEST ///
setRandomSeed 1
C=CC_200
C[x,y,z]
F = polySystem {x^3,y^3,x^2*y,z*(z-1)^2}
P = point sub(matrix{{0.000001, 0.000001*ii,1.000001-0.000001*ii}},C)
deflateInPlace(P,F)
assert(P.DeflationSequence == {0,1})
assert(10{*a not too large constant*}*P.ErrorBoundEstimate^2 > (newton(P.LiftedSystem,P.LiftedPoint)).ErrorBoundEstimate)
///

partitionViaDeflationSequence = method()
partitionViaDeflationSequence (List,PolySystem) := (pts,F) -> (
    H := new MutableHashTable;
    for p in pts do (
	if not p.?DeflationSequence or p.SolutionSystem =!= F 
	then deflateInPlace(p,F);
	ds := p.DeflationSequence;
	if H#?ds then H#ds = H#ds | {p}
	else H#ds = {p}; 
	);
    values H
    )

TEST ///
C=CC_200
C[x,y,z]
F = polySystem {x^3,y^3,x^2*y,z*(z^2-1)^2}
e = 0.0000001
pts = {
    point sub(matrix{{e, e*ii,e-e*ii}},C),
    point sub(matrix{{e, e*ii,1+e-e*ii}},C),
    point sub(matrix{{e, e*ii,-1-e-e*ii}},C)
    }    
debug NumericalAlgebraicGeometry
partitionViaDeflationSequence(pts,F)
///

--------------------------------
-- OLD deflation
--------------------------------
///
dMatrix = method()
dMatrix (List,ZZ) := (F,d) -> dMatrix(ideal F, d)
dMatrix (Ideal,ZZ) := (I, d) -> (
-- deflation matrix of order d     
     R := ring I;
     v := flatten entries vars R;
     n := #v;
     ind := toList((n:0)..(n:d)) / toList;
     ind = select(ind, i->sum(i)<=d and sum(i)>0);
     A := transpose diff(matrix apply(ind, j->{R_j}), gens I);
     scan(select(ind, i->sum(i)<d and sum(i)>0), i->(
	       A = A || transpose diff(matrix apply(ind, j->{R_j}), R_i*gens I);
	       ));
     A
     )
dIdeal = method()
dIdeal (Ideal, ZZ) := (I, d) -> (
-- deflation ideal of order d     
     R := ring I;
     v := gens R;
     n := #v;
     ind := toList((n:0)..(n:d)) / toList;
     ind = select(ind, i->sum(i)<=d and sum(i)>0);
     A := dMatrix(I,d);
     newvars := apply(ind, i->getSymbol("x"|concatenate(i/toString)));
     S := (coefficientRing R)[newvars,v]; 
     sub(I,S) + ideal(sub(A,S) * transpose (vars S)_{0..#ind-1})
     )	   
deflatedSystem = method()
deflatedSystem(Ideal, Matrix, ZZ, ZZ) := memoize (
(I, M, r, attempt) -> (
-- In: gens I = the original (square) system   
--     M = deflation matrix
--     r = numerical rank of M (at some point)
-- Out: (square system of n+r equations, the random matrix SM)
     R := ring I;
     n := numgens R;
     SM := randomOrthonormalCols(numcols M, r+1);
     d := local d;
     S := (coefficientRing R)(monoid[gens R, d_0..d_(r-1)]);
     DF := sub(M,S)*sub(SM,S)*transpose ((vars S)_{n..n+r-1}|matrix{{1_S}}); -- new equations
     --print DF;     
     (
	  flatten entries squareUp ( sub(transpose gens I,S) || DF ),
	  SM
	  )
     )
) -- END memoize

liftSolution = method(Options=>{Tolerance=>null}) -- lifts a solution s to a solution of a deflated system dT (returns null if unsuccessful)
liftSolution(List, List) := o->(s,dT)->liftSolution(s, transpose matrix{dT},o)
liftSolution(List, Matrix) := o->(c,dT)->(
     R := ring dT;
     n := #c;
     N := numgens R;
     if N<=n then error "the number of variables in the deflated system is expected to be larger"; 
     newVars := (vars R)_{n..N-1};
     specR := (coefficientRing R)(monoid[flatten entries newVars]);
     dT0 := (map(specR, R, matrix{c}|vars specR)) dT;
     ls := first solveSystem flatten entries squareUpSystem dT0; -- here a linear system is solved!!!     
     if status ls =!= Regular then return null;
     ret := c | coordinates ls;
     -- if norm sub(dT, matrix{ret}) < o.Tolerance * norm matrix{c} then ret else null
     ret
     ) 
///
