------------------------------------------------------
-- witness set manipulation routines 
-- (loaded by  ../NumericalAlgebraicGeometry.m2)
------------------------------------------------------

WitnessSet.Tolerance = 1e-6;
check WitnessSet := o -> W -> for p in points W do if norm sub(matrix{equations W | slice W}, matrix {p})/norm p > 1000*DEFAULT.Tolerance then error "check failed" 

isOn (Point,WitnessSet) := o -> (p, W) -> (
    o = fillInDefaultOptions o;
    --if o.Software === BERTINI then bertiniComponentMemberTest(numericalWariety {W},{p})
    --else 
    (
	R := ring W.Equations;
	C := coefficientRing R;
	M := random(C^(dim W), C^(numgens R));
	W' := moveSlice(W, M | -M*transpose matrix p);
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
    
isSubset (WitnessSet,WitnessSet) := (V,W) -> (
     coD := dim W - dim V;
     coD >= 0
     and all(points V, p->isOn(p,W))
     )

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

randomSlice = method()
randomSlice (ZZ,ZZ) := (d,n) -> (randomUnitaryMatrix n)^(toList(0..d-1)) | random(CC^d,CC^1)   
randomSlice (ZZ,ZZ,Point) := (d,n,point) -> (
     SM := (randomUnitaryMatrix n)^(toList(0..d-1));
     SM | (-SM * transpose matrix point)
     )

movePoints = method(Options=>{AllowSingular=>false, Software=>null})
movePoints (List, List, List, List) := List => o -> (E,S,S',w) -> (
-- IN:  E = equations, 
--      S = equations of the current slice,
--      S' = equations of the new slice,
--      w = points satisfying E and S (in the output format of track) 
--      AllowSingular => false: S' is generic, several attempts are made to get regular points
-- OUT: new witness points satisfying E and S'
     o = fillInDefaultOptions o;
     attempts := DEFAULT.Attempts;
     success := false;
     P := first w; -- all witness points are supposed to have the same "multiplicity structure"
     local w';
     if status P === Singular then (
	 seq := P.DeflationSequenceMatrices;
	 F := squareUp P.LiftedSystem; -- assumes P.System == E|S, in particular
	 ES' := polySystem(E|S');
	 F' := squareUp(deflate(ES', seq), P.LiftedSystem.SquareUpMatrix); -- square-up using the same matrix
	 );
     while (not success and attempts > 0) do (
	  attempts = attempts - 1;
	  if status P =!= Singular
	  then (
	      w' = track(E|S, E|S', w, NumericalAlgebraicGeometry$gamma=>exp(random(0.,2*pi)*ii));
	      success = all(w', p->status p === Regular);
    	      )
	  else (
	      assert all(w, p->p.LiftedSystem===P.LiftedSystem);
	      F'.PolyMap = (map(ring F, ring F', vars ring F)) F'.PolyMap; -- hack: rewrite with trackHomotopy
	      lifted'w' := track(F, F', w/(p->p.LiftedPoint), NumericalAlgebraicGeometry$gamma=>exp(random(0.,2*pi)*ii));
	      if success = all(lifted'w', p->status p === Regular) 
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
     if attempts == 0 and not success then error "some path is singular generically";  
     w'
     )

movePointsToSlice = method(TypicalValue=>WitnessSet, Options=>{Software=>null})
movePointsToSlice (WitnessSet, List) := List => o -> (W,S') -> (
-- IN:  W = witness set
--      S' = equations of the new slice
-- OUT: new witness points
     o = fillInDefaultOptions o;
     if #S' < dim W
     then error "dimension of new slicing plane is too high";
     R := ring W;
     S := take(slice W,-#S'); -- take last #S equations
     movePoints(equations W, S, S', W.Points, AllowSingular=>true, Software=>o.Software)
     )

moveSlice = method(TypicalValue=>WitnessSet, Options=>{Software=>null})
moveSlice (WitnessSet, Matrix) := WitnessSet => o->(W,S) -> (
-- IN:  W = witness set
--      S = matrix defining a new slicing plane (same dimensions as W#Slice)
-- OUT: new witness set that uses S
     o = fillInDefaultOptions o;
     if numgens target S != numgens target W#Slice 
     or numgens source S != numgens source W#Slice 
     then error "wrong dimension of new slicing plane";
     witnessSet(W#Equations,S,movePointsToSlice(W,sliceEquations(S,ring W),Software=>o.Software))             	  
     )

-- get a random Point(s) 
random WitnessSet := o -> W -> (
    W' := moveSlice(W, randomSlice(dim W, numgens ring W));
    W'.Points # (random(#W'.Points)) 
    )

TEST ///
restart 
debug needsPackage "NumericalAlgebraicGeometry"
R = CC[x,y,z]
W1 = new WitnessSet from {
     Equations=>ideal {x^2+y^2+z^2-1},
     Slice=>matrix "1,0,0,0;0,1,0,0",
     Points=>{{{0,0,1}},{{0,0,-1}}}/point
     } 
sliceEquations (W1#Slice,R)
W2 = moveSlice(W1, matrix "0,1,0,0;0,0,1,0")
assert areEqual(sortSolutions points W2, {point{{ -1,0,0}},point{{1,0,0}}})
for i to 5 do assert isOn(random W1,W1)
///



