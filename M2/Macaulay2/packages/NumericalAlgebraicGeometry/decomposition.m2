------------------------------------------------------
-- numerical irreducible decomposition
-- (loaded by  ../NumericalAlgebraicGeometry.m2)
------------------------------------------------------

regeneration = method(TypicalValue=>NumericalVariety, Options =>{Software=>null, Output=>Singular})
regeneration List := List => o -> F -> (
-- solves a system of polynomial Equations via regeneration     
-- IN:  F = list of polynomials
--      Software => {PHCPACK, BERTINI, hom4ps2}
-- OUT: a NumericalVariety
     o = fillInDefaultOptions o;
     
     checkCCpolynomials F;
     F = toCCpolynomials(F,53);
     
     R := ring F#0;
     V := numericalAffineSpace R; -- current solution components
     for f in F do V = hypersurfaceSection(V,f,o); -- intersect with hypersurface	 
     V
     )

TEST /// -- example with a non-reduced component
setRandomSeed 0
R = CC[x,y,z]
sph = (x^2+y^2+z^2-1); 
I = ideal {sph, (x+y+z-1)^2};
result = regeneration I_* -- deflation sequences are supposed to be the same
assert(dim result == 1 and degree result#1#0 == 2)
p = first points result#1#0
assert(numericalRank evaluate(jacobian p.cache.SolutionSystem,p) == 2)
///

-----------------------------------------------------------------------
-- DECOMPOSITION
decompose WitnessSet := {} >> unusedOpts -> (W) -> (
     R := ring W;
     n := numgens R;
     k := dim W;
     eq := equations W;
     which := new MutableHashTable from {}; 
     cs := new MutableList from apply(degree W, i->(which#i = i; {i})); -- current components
     i'cs := {}; -- certified irreducible components
     for i from 0 to #cs-1 do if linearTraceTest(W, cs#i) then (i'cs = i'cs | {cs#i}; cs#i = {}) ;
     --sorted'cs := MutableList toList(0..deg W - 1); -- list of numbers of components sorted by degree (small to large)
     -- -1 indicates no component 
     mergeComponents := (c,c') -> (
	  cs#c = cs#c | cs#c';
	  cs#c' = {};
	  );	     	
     findComponent := (pt) -> ( 
	 for i to #cs-1  do (
	     if any(cs#i, p->areEqual((points W)#p,pt)) 
	     then return i; 
	     );
	 return null 
	 );
     done := all(new List from cs, c->#c==0);
     n'misses := 0;
     while not done do (
	  while (c := random(#cs); #cs#c == 0) do (); -- vvv
	  p := cs#c#(random(#(cs#c))); -- pick a component/point (rewrite!!!)
	  S := eq | slice W;	  
	  while (
	      T := sliceEquations(randomSlice(k,n),R); 
	      pt' := first movePoints(W, slice W, T, {(W.Points)#p}, Software=>M2engine);
	      status pt' =!= Regular
	      ) 
	  do (); 
	  pt := first movePoints(W, T, slice W, {pt'}, Software=>M2engine);
	  if (c' :=  findComponent pt) === null then error "point outside of any current component";
	  if c' == c then n'misses = n'misses + 1
	  else ( 
	       mergeComponents(c,c');
	       if linearTraceTest(W, cs#c) then (i'cs = i'cs | {cs#c}; cs#c = {});  
	       n'misses = 0 );
	  done = all(new List from cs, c->#c==0) or n'misses > 30;
	  );
     incomplete := select(new List from cs, c->#c!=0);
     if #incomplete>0 then print "-- decompose: some witness points were not classified";
     irred := apply(i'cs, c->witnessSet(W.Equations, W.Slice, (W.Points)_c));
     scan(irred, c->c.cache.IsIrreducible = true);
     irred | if #incomplete == 0 then {} 
             else {
		 witnessSet(
		     W.Equations, 
		     W.Slice, 
		     (W.Points)_(flatten(incomplete))
		     )
		 }
     ) 

linearTraceTest = method() -- check linearity of trace to see if component is irreducible
linearTraceTest (WitnessSet, List) := (W,c) -> (
-- IN: W = witness superset, 
--     c = list of integers (witness points subset)
-- OUT: do (points W)_c represent an irreducible component?
     if dim W == 0 then return true;
     w := (W.Points)_c;
     proj := random(CC^(numgens ring W), CC^1); 
     three'samples := apply(3, i->(
	       local r;
	       w' := (
		    if i == 0 then (
 			-- !!! better to NOT use the existing slice
			r = W.Slice_(dim W - 1, numgens ring W);
			w 
			)
		    else (
	       	    	 M := new MutableMatrix from W.Slice;
		    	 M_(dim W - 1, numgens ring W) = r = random CC; -- replace last column
		    	 movePoints(W, slice W, sliceEquations(matrix M,ring W), w, Software=>M2engine) 
	       	    	 ) );
	       {1, r, sum flatten entries (matrix (w'/coordinates) * proj)} 
               ));
     if DBG>2 then (
	  print matrix three'samples;
     	  print det matrix three'samples;
	  );
     abs det matrix three'samples < DEFAULT.Tolerance  -- points are (approximately) on a line
     )  

TEST ///
setRandomSeed 0
R = CC[x,y]
F = {x^2+y^2-1, x*y};
result = components regeneration F 
assert(#result==1 and degree first result == 4 and dim first result == 0)

--example with a reduced scheme (no singular points)
setRandomSeed 0
R = CC[x,y,z]
sph = (x^2+y^2+z^2-1); 
I = ideal {sph*(x-1)*(y-x^2), sph*(y-2)*(z-x^3)};
result = components regeneration I_*
assert(#result==2 and result/degree == {7,2} and result/dim == {1,2})

--example with 4 double points (same deflation sequence)
-- NAGtrace 3
R = CC[x,y,z];
sph = (x^2+y^2+z^2-1); 
I = ideal {sph*(y-x^2), sph*(z-x^3), (x+y+z-1)^2}; -- ^3 fails
for i to 5 do (
    print setRandomSeed i;
    result = components regeneration I_*;
    assert(#result==2 and result/degree == {3,2} and result/dim == {0,1})
    )
///


numericalIrreducibleDecompositionM2 = (I,o) -> numericalVariety flatten (components regeneration (I_*,Software=>M2engine) / decompose)
numericalIrreducibleDecomposition = method(Options=>{Software=>null})
numericalIrreducibleDecomposition Ideal := o -> I -> (
    o = fillInDefaultOptions o;   
    ( 
	if o.Software === BERTINI 
    	then numericalIrreducibleDecompositionBertini 
    	else if o.Software === PHCPACK 
    	then numericalIrreducibleDecompositionPHCpack 
    	else if member(o.Software,{M2,M2engine})  
	then numericalIrreducibleDecompositionM2
    	else error "allowed values for Software: M2engine, M2, BERTINI, PHCPACK"
    	) (I,o)
    )

TEST /// -- example with one nonreduced component
setRandomSeed 0
R = CC[x,y,z]
sph = (x^2+y^2+z^2-1); 
I = ideal {sph*(x-1)*(y-x^2), sph*(y-1)*(z-x^3)};
V = numericalIrreducibleDecomposition I 
assert(#V#1==4 and sort(V#1/degree) == {1, 1, 1, 3} and #V#2==1 and degree first V#2 == 2)
///

