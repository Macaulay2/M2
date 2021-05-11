------------------------------------------------------
-- witness set manipulation routines 
-- (loaded by  ../NumericalAlgebraicGeometry.m2)
------------------------------------------------------

export { 
    "isOn",
    "sample", 
    "union", -- aka "|"
    "removeRedundantComponents"
    }

polySystem WitnessSet := W->if W.cache.?SolutionSystem then W.cache.SolutionSystem else 
    W.cache.SolutionSystem = polySystem(
    	n := #equations W;
    	R := ring W;
    	m := codim W;
	if m == n then W.Equations else (
    	    M := sub(randomOrthonormalRows(m,n),coefficientRing R);
    	    sub(M,ring W) * transpose matrix{equations W}
	    )
    	)

check WitnessSet := o -> W -> for p in W.Points do 
if residual(polySystem(equations polySystem W | slice W), p) > 1000*DEFAULT.Tolerance then error "check failed" --!!!

randomSlice = method()
randomSlice (ZZ,ZZ,Ring) := (d,n,C) -> (randomUnitaryMatrix n)^(toList(0..d-1)) | random(C^d,C^1)   
randomSlice (ZZ,ZZ,Ring,Point) := (d,n,C,point) -> (
     SM := (randomUnitaryMatrix n)^(toList(0..d-1));
     SM | (-SM * transpose matrix point)
     )
randomSlice (ZZ,ZZ) := (d,n) -> randomSlice(d,n,CC_53)
randomSlice (ZZ,ZZ,Point) := (d,n,point) -> randomSlice(d,n,CC_53,point)

isOn = method(Options=>{Tolerance=>null,Software=>null})
isOn (Point,WitnessSet) := o -> (p, W) -> (
    o = fillInDefaultOptions o;
    if # coordinates p != numgens ring W 
    then if W.cache.?ProjectionDimension then isOn(p,W,W.ProjectionDimension,o) 
    else error "numbers of coordinates mismatch";
    --if o.Software === BERTINI then bertiniComponentMemberTest(numericalWariety {W},{p})
    --else 
    (
	R := ring W.Equations;
	C := coefficientRing R;
	W' := moveSlice(W, randomSlice(dim W, numgens R, C, p));
	any(W'.Points, p'->areEqual(p',p))
	)
    )

-- checks if the projection of W to the first m coordinates contains p
isOn (Point,WitnessSet,ZZ) := o -> (p, W, m) -> (
    o = fillInDefaultOptions o;
    if # coordinates p != m then error "wrong number of coordinates",
    R := ring W.Equations;                    
    C := coefficientRing R;
    n := numgens R;
    rows := toList (0..dim W - 1);
    M := ((map C^n)^m || random(C^(n-m),C^n))^rows;
    mp := transpose (matrix p | random(C^1,C^(n-m)));
    W' := moveSlice(W, M | -M*mp);
    any(W'.Points, p'->areEqual(project(p',m),p))
    )

-- ... hypersurface V(f)
isOn (Point,RingElement) := o -> (p, f) ->  isOn (p, witnessSet(ideal f, numgens ring f - 1), o)

-- ... V(I)
isOn (Point,Ideal) := o -> (p, I) -> all(I_*, f->isOn(p,f,o))
        
isSubset (WitnessSet,WitnessSet) := (V,W) -> (
     coD := dim W - dim V;
     coD >= 0 and isOn(random V,W)
     )
WitnessSet == WitnessSet := (V,W)->isSubset(V,W) and isSubset(W,V)

TEST /// -- isOn
R = CC[x,y]	
I = ideal((x^2+y^2+2)*x,(x^2+y^2+2)*y);
e = 0.0000001
W = witnessSet(ideal I_0 , ideal(x-y), {
	point {{ (1-e)*ii,(1-e)*ii}}, 
	point {{ -(1+e)*ii,-(1+e)*ii}}
	} )
assert isOn(point {{sqrt 5*ii,sqrt 3}},W)
assert not isOn(point {{sqrt 5*ii,1.7}},W)
assert isOn(point{{1+e}},W,1)
assert isOn(point{{ii+e,ii+e}},W,2)
assert not isOn(point{{1,1}},W,2)
///

WitnessSet - WitnessSet := (V,W) -> ( -- difference V/W, also used to remove junk points
     coD := dim W - dim V;
     if coD < 0 then V
     else witnessSet(V.Equations, V.Slice, select(V.Points, p->not member(coordinates p,W)))
     ) 

movePoints = method(Options=>{Software=>null})
movePoints (WitnessSet, List, List, List) := List => o -> (W,S,S',w) -> (
-- IN:  W = a witness set  
--      S = equations of the current slice,
--      S' = equations of the new slice,
--      w = a subset of the intersection of V(W) and V(S)
-- OUT: new witness points in the intersection of V(W) and V(S')
     o = fillInDefaultOptions o;

     attempts := DEFAULT.Attempts;
     success := false;
     P := first w; -- all witness points are supposed to have the same "multiplicity structure"
     local w';
     E := equations polySystem W;
     if status P === Singular then (
	 seq := P.DeflationSequenceMatrices;
	 F := squareUp P.LiftedSystem; -- assumes P.SolutionSystem == equations W.SolutionSystem | S
	 ES' := polySystem(E|S');
	 F' := squareUp(deflate(ES', seq), squareUpMatrix P.LiftedSystem); -- square-up using the same matrix
	 );
     while (not success and attempts > 0) do (
	 attempts = attempts - 1;
	 if status P =!= Singular
	 then (
	     w' = --refine(E|S', 
		 track(E|S, E|S', w, 
		     NumericalAlgebraicGeometry$gamma=>exp(random(0.,2*pi)*ii), Software=>o.Software)
		 --)
	     ;
	     success = all(w', p->member(status p, {Regular-*,Singular*-}));
	     )
	 else (
	     assert all(w, p->p.LiftedSystem===P.LiftedSystem); -- !!!
	     F'.PolyMap = (map(ring F, ring F', vars ring F)) F'.PolyMap; -- hack!!!: rewrite with trackHomotopy
	     lifted'w' := --refine(F',
	     	 track(F, F', w/(p->p.LiftedPoint), 
		     NumericalAlgebraicGeometry$gamma=>exp(random(0.,2*pi)*ii), Software=>o.Software)
		 --)
		 ;
	     if success = all(lifted'w', p->member(status p, {Regular-*,Singular*-})) 
	     then w' = apply(lifted'w', p->(
		     q := new Point from P;
		     q.System = ES';
		     q.LiftedSystem = F';
		     q.LiftedPoint = p;
		     q.Coordinates = take(coordinates p, ES'.NumberOfVariables);
		     q
		     ));
	     );
	 );
     if attempts == 0 and not success then error "ran out of attempts to move witness points";  

     w'
     )


moveSlicingVariety(WitnessSet,SlicingVariety) := (W,S) -> moveSlice(W,flatten entries map S)

moveSlice = method(TypicalValue=>WitnessSet, Options=>{Software=>null})
moveSlice (WitnessSet, List) := List => o -> (W,S') -> (
-- IN:  W = witness set
--      S' = equations of the new slice
-- OUT: new witness points
     o = fillInDefaultOptions o;
     if #S' < dim W
     then error "dimension of new slicing plane is too high";
     w' := movePoints(W, slice W, S', W.Points, o);
     witnessSet(W.Equations, sliceEquationsToMatrix ideal S', w')
     )

moveSlice (WitnessSet, Matrix) := WitnessSet => o->(W,S) -> (
-- IN:  W = witness set
--      S = matrix defining a new slicing plane (same dimensions as W#Slice)
-- OUT: new witness set that uses S
     o = fillInDefaultOptions o;
     if numgens target S != numgens target W#Slice 
     or numgens source S != numgens source W#Slice 
     then error "wrong dimension of new slicing plane";
     moveSlice(W,sliceEquations(S,ring W),o)             	  
     )

-- get a random Point
sample = method(Options=>{Tolerance=>1e-6})
sample WitnessSet := o -> W -> (
    W' := moveSlice(W, randomSlice(dim W, numgens ring W, coefficientRing ring W));
    p := W'.Points # (random(#W'.Points));
    if not p.?ErrorBoundEstimate or p.ErrorBoundEstimate > o.Tolerance then p = refine(p,ErrorTolerance=>o.Tolerance); 
    if W.cache.?ProjectionDimension then project(p, W.cache.ProjectionDimension)
    else p 
    )
random WitnessSet := o -> W -> sample W

TEST ///
debug needsPackage "NumericalAlgebraicGeometry"
R = CC[x,y,z]
W1 = witnessSet(
     ideal {x^2+y^2+z^2-1},
     matrix "1,0,0,0;0,1,0,0",
     {{{0,0,1}},{{0,0,-1}}}/point
     )
sliceEquations (W1#Slice,R)
W2 = moveSlice(W1, matrix "0,1,0,0;0,0,1,0")
assert areEqual(sortSolutions points W2, {point{{ -1,0,0}},point{{1,0,0}}})
for i to 5 do assert isOn(random W1,W1)

W3 = witnessSet(
     ideal {x^2+y^2+z^2-1, z},
     matrix "1,0,0,0",
     {{{0,1,0}},{{0,-1,0}}}/point
     )
W3.cache.ProjectionDimension = 2 -- project onto xy-plane

for i to 5 do assert isOn(random W3,W3,2)

W4 = witnessSet(
     ideal {x^2+y^2+z^2-1, z^2},
     matrix "1,0,0,0",
     {{{0,1,0_CC}},{{0,-1,0_CC}}}/point
     ) 
F = polySystem(equations W4 | slice W4)
scan(W4.Points, P->deflateInPlace(P,F))
for i to 5 do assert isOn(random W4,W4)

P = sample(W3, Tolerance=>1e-15)
assert(P.ErrorBoundEstimate < 1e-15) 
P = sample(W4, Tolerance=>1e-15)
assert(P.ErrorBoundEstimate < 1e-15) 
///

-- a constructor for witnessSet that depends on NAG
witnessSet (Ideal,ZZ) := (I,d) -> ( -- assume: dim I == d
     R := ring I;
     n := numgens R;
     F := if numgens I == n-d then I_* else (
     	 RM := (randomUnitaryMatrix numgens I)^(toList(0..n-d-1));
     	 RM = promote(RM,ring I);
     	 flatten entries (RM * transpose gens I) 
	 );
     SM := (randomUnitaryMatrix n)^(toList(0..d-1))|random(CC^d,CC^1);
     S := ideal(promote(SM,R) * ((transpose vars R)||matrix{{1_R}}));
     P := solveSystem(F | S_*);
     w'points := if numgens I == n-d then P else select(P, p->isOn(p,I));
     witnessSet(ideal F, SM, w'points)
     )

TEST ///
CC[x,y,z]
I = ideal (x^2+y)
W = witnessSet (I,2)
assert(dim W == 2 and degree W == 2)
I = ideal (x,y,z*(z-1))
assert( degree witnessSet(I,0) == 2 )
///

isSubset(WitnessSet,NumericalVariety) := (W,V) -> any(components V, W'->isSubset(W,W')) 
isSubset(NumericalVariety,NumericalVariety) := (A,B) -> all(components A, W->isSubset(W,B)) 
NumericalVariety == NumericalVariety := (A,B) -> isSubset(A,B) and isSubset(B,A)

union = method()
union (NumericalVariety, NumericalVariety) := (A,B) -> new NumericalVariety from 
  merge(new HashTable from A, new HashTable from B,(a,b)->a|b)
NumericalVariety | NumericalVariety := union

removeRedundantComponents = method(Options=>{Tolerance=>null})
removeRedundantComponents NumericalVariety := o -> V -> (
    o = fillInDefaultOptions o;
    scan(rsort keys V, d->(
	Vd := V#d;
	ind := select(#Vd, i->
	    all(components(V,d+1,infinity), W->not isSubset(Vd#i,W))
	    and 
	    all(drop(Vd,i+1), W->not isSubset(Vd#i,W))
	    );
	V#d = Vd_ind;
	))
    )

TEST ///
CC[x,y,z]
V2 = numericalVariety { witnessSet(ideal (x^2+y), 2) }
V1 = numericalVariety { witnessSet(ideal (x^2-y,z), 1) }
-- V0 = numericalVariety decompose witnessSet(ideal (x,y,z*(z-1)), 0) -- !!! problem with tracking to the origin
V0 = numericalVariety decompose witnessSet(ideal ((x-1)*x,y-1,z), 0) 

VV = V2 | V2 | V1 | V0
removeRedundantComponents VV
assert (#components VV ==3 and keys VV == {0,1,2})
///
 
