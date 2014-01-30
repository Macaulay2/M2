------------------------------------------------------
-- witness set manipulation routines 
-- (loaded by  ../NumericalAlgebraicGeometry.m2)
------------------------------------------------------

WitnessSet.Tolerance = 1e-6;
check WitnessSet := o -> W -> for p in points W do if norm sub(matrix{equations W | slice W}, matrix {p})/norm p > 1000*DEFAULT.Tolerance then error "check failed" 
isContained = method()
isContained (List,WitnessSet) := (point,W) -> (
     pts := movePointsToSlice(W, sliceEquations(randomSlice(dim W, numgens ring W, point),ring W)) / coordinates;
     any(pts, p->areEqual(point,p,Tolerance=>WitnessSet.Tolerance))
     )
isContained (WitnessSet,WitnessSet) := (V,W) -> (
     coD := dim W - dim V;
     coD >= 0
     and all(points V, p->isContained(p,W))
     )
-- subtract = method()
-- subtract (WitnessSet, WitnessSet) 
WitnessSet - WitnessSet := (V,W) -> ( -- difference V/W, also used to remove junk points
     coD := dim W - dim V;
     if coD < 0 then V
     else witnessSet(V.Equations, V.Slice, select(V.Points, p->not isContained(coordinates p,W)))
     ) 
///
restart
debug loadPackage "NumericalAlgebraicGeometry"
CC[x,y,z]
I = ideal (x^2+y)
S = ideal (x+y+2*z-1)
P = {{ii_CC,1_CC},{ii_CC,1_CC}}
I = ideal {z-x*y, x^2-y, y^2-z*x}
W = witnessSet(I,S,P)
W = witnessSet I
W = witnessSet(I, sub(transpose last coefficients gens S,CC), P)
points W
equations W
slice W
///

randomSlice = method()
randomSlice (ZZ,ZZ) := (d,n) -> randomSlice(d,n,{})
randomSlice (ZZ,ZZ,List) := (d,n,point) -> (
     SM := (randomUnitaryMatrix n)^(toList(0..d-1));
     SM | (if #point==0
	  then random(CC^d,CC^1)    
	  else -SM * transpose matrix{point} -- slice goes thru point
	  )
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
///
restart
debug loadPackage "NumericalAlgebraicGeometry"
R = CC[x,y,z]
W1 = new WitnessSet from {
     Equations=>ideal {x^2+y^2+z^2-1},
     Slice=>matrix "1,0,0,0;0,1,0,0",
     Points=>{{(0,0,1)},{(0,0,-1)}}/point
     } 
sliceEquations (W1#Slice,R)
W2 = moveSlice(W1, matrix "0,1,0,0;0,0,1,0")
peek W2
///

