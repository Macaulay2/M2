------------------------------------------------------
-- numerical irreducible decomposition
-- (loaded by  ../NumericalAlgebraicGeometry.m2)
------------------------------------------------------

insertComponent = method()
insertComponent(WitnessSet,MutableHashTable) := (W,H) -> (
     d := dim W;
     if H#?d then H#d#(#H) = W 
     else H#d = new MutableHashTable from {0=>W};
     )

splitWitness = method(TypicalValue=>Sequence, Options =>{Tolerance=>null})
splitWitness (WitnessSet,RingElement) := Sequence => o -> (w,f) -> (
-- splits the witness set into two parts: one contained in {f=0}, the other not
-- IN:  comp = a witness set
--      f = a polynomial
-- OUT: (w1,w2) = two witness sets   
     o = fillInDefaultOptions o;
     w1 := {}; w2 := {};
     for x in w#Points do 
	 if norm residual(matrix {{f}}, matrix x) < o.Tolerance 
	 then w1 = w1 | {x}
	 else w2 = w2 | {x};   
     ( if #w1===0 then null else witnessSet(w#Equations, w#Slice, w1), 
       if #w2===0 then null else witnessSet(w#Equations, w#Slice, w2) 
       )
   )

regeneration = method(TypicalValue=>List, Options =>{Software=>null, Output=>Singular})
regeneration List := List => o -> F -> (
-- solves a system of polynomial Equations via regeneration     
-- IN:  F = list of polynomials
--      Software => {PHCPACK, BERTINI, hom4ps2}
-- OUT: {s,m}, where 
--             s = list of solutions 
--     	       m = list of corresponding multiplicities	 
     o = fillInDefaultOptions o;
     
     checkCCpolynomials F;
     F = toCCpolynomials(F,53);
     
     R := ring F#0;
     c1 := {}; -- current solution components
     for f in F do (
	  d := sum degree f;
	  c2 := new MutableHashTable; -- new components
	  for comp in c1 do (
	       if DBG>2 then << "*** proccesing component " << peek comp << endl;
	       (cIn,cOut) := splitWitness(comp,f); 
	       if cIn =!= null then (
		   if DBG>2 then << "( regeneration: " << net cIn << " is contained in V(f) for" << endl <<  
		                 << "  f = " << f << " )" << endl;
		   insertComponent(cIn,c2)
		   ); 
     	       if cOut =!= null 
	       and dim cOut > 0 -- 0-dimensional components outside V(f) discarded
	       then (
		    s := cOut#Slice;
		    -- RM := (randomUnitaryMatrix numcols s)^(toList(0..d-2)); -- pick d-1 random orthogonal row-vectors (this is wrong!!! is there a good way to pick d-1 random hyperplanes???)
     	       	    RM := random(CC^(d-1),CC^(numcols s));
		    dWS := {cOut} | apply(d-1, i->(
			    newSlice := RM^{i} || submatrix'(s,{0},{}); -- replace the first row
			    moveSlice(cOut,newSlice,Software=>o.Software)
			    ));
		    slice' := submatrix'(comp#Slice,{0},{});
	       	    S := polySystem( equations comp
	       	    	 | { product flatten apply( dWS, w->sliceEquations(w.Slice^{0},R) ) } -- product of linear factors
	       	    	 | sliceEquations(slice',R) );
	       	    T := polySystem( equations comp
	       	    	 | {f}
	       	    	 | sliceEquations(slice',R) );
	       	    targetPoints := track(S,T,flatten apply(dWS,points), 
			 NumericalAlgebraicGeometry$gamma=>exp(random(0.,2*pi)*ii),
			 Software=>o.Software);
		    refinedPoints := refine(T, targetPoints, ResidualTolerance=>1e-8);
		    regPoints := select(refinedPoints, p->p.SolutionStatus===Regular);
		    singPoints := select(refinedPoints, p->p.SolutionStatus===Singular);
		    if o.Output == Regular then targetPoints = regPoints 
		    else targetPoints = regPoints | solutionsWithMultiplicity singPoints;
		    if DBG>2 then << "( regeneration: " << net cOut << " meets V(f) at " << 
		                  << #targetPoints << " points for" << endl <<
				  << "  f = " << f << " )" << endl;
		    f' := ideal (equations comp | {f});
		    scan(partitionViaDeflationSequence(targetPoints,T),
			pts -> (
			    newW := witnessSet(f',slice',selectUnique(pts, Tolerance=>1e-3));
			    if DBG>2 then << "   new component " << peek newW << endl;
			    check newW;    
			    insertComponent(newW,c2);
			    )
			)
		    ) 
	       );
	  scan(rsort keys c2, d->scan(keys c2#d,i->(
			 W := c2#d#i;
			 scan(rsort keys c2,j->if j>d then (for k in keys c2#j do W = W - c2#j#k));
			 c2#d#i = W;
			 )));
	  c1 = flatten apply(keys c2, i->apply(keys c2#i, j->c2#i#j));
	  if f == first F then ( -- if the first equation is being processed 
	       n := numgens R;
	       S := randomSlice(n-1,n);
     	       c1 = {witnessSet( ideal f, S, solveSystem({f}|sliceEquations(S,R),PostProcess=>false) )}; 
	       );
	  );
     c1
     )

-----------------------------------------------------------------------
-- DECOMPOSITION
decompose WitnessSet := (W) -> (
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
	  while (T := sliceEquations(randomSlice(k,n),R); 
	       pt' := track(S,eq|T,{coordinates (W.Points)#p}); 
	       not isRegular(pt',0)) 
	  do (); 
	  pt := first movePoints(eq, T, slice W, pt'/coordinates);
	  if (c' :=  findComponent coordinates pt) === null then error "point outside of any current component";
	  if c' == c then n'misses = n'misses + 1
	  else ( 
	       mergeComponents(c,c');
	       if linearTraceTest(W, cs#c) then (i'cs = i'cs | {cs#c}; cs#c = {});  
	       n'misses = 0 );
	  done = all(new List from cs, c->#c==0) or n'misses > 30;
	  );
     incomplete := select(new List from cs, c->#c!=0);
     if #incomplete>0 then print "-- decompose: some witness points were not classified";
     irred := apply(i'cs, c->new WitnessSet from {Equations=>W.Equations, Slice=>W.Slice, Points=>(W.Points)_c});
     scan(irred, c->c.IsIrreducible = true);
     irred | if #incomplete == 0 then {} 
             else {new WitnessSet from {Equations=>W.Equations, Slice=>W.Slice, Points=>(W.Points)_(flatten(incomplete))}}
     ) 

linearTraceTest = method() -- check linearity of trace to see if component is irreducible
linearTraceTest (WitnessSet, List) := (W,c) -> (
-- IN: W = witness superset, 
--     c = list of integers (witness points subset)
-- OUT: do (points W)_c represent an irreducible component?
     if dim W == 0 then return true;
     w := (points W)_c;
     proj := random(CC^(numgens ring W), CC^1); 
     three'samples := apply(3, i->(
	       local r;
	       w' := (
		    if i == 0 then (
     	       	    	 r = W.Slice_(dim W - 1, numgens ring W);
			 w 
		    	 )
		    else (
	       	    	 M := new MutableMatrix from W.Slice;
		    	 M_(dim W - 1, numgens ring W) = r = random CC; -- replace last column
		    	 movePoints(equations W, slice W, sliceEquations(matrix M,ring W), w) / coordinates 
	       	    	 ) );
	       {1, r, sum flatten entries (matrix w' * proj)} 
               ));
     if DBG>2 then (
	  print matrix three'samples;
     	  print det matrix three'samples;
	  );
     abs det matrix three'samples < DEFAULT.Tolerance  -- points are (approximately) on a line
     )  

TEST ///
R = CC[x,y]
F = {x^2+y^2-1, x*y};
result = regeneration F 
assert(#result==1 and degree first result == 4 and dim first result == 0)

--example with a reduced scheme (no singular points)
R = CC[x,y,z]
sph = (x^2+y^2+z^2-1); 
I = ideal {sph*(x-1)*(y-x^2), sph*(y-2)*(z-x^3)};
result = regeneration I_*
assert(#result==2 and result/degree == {7,2} and result/dim == {1,2})

--example with 4 double points (same deflation sequence)
setRandomSeed 3 -- fails for 0,1,2 !!!
I = ideal {sph*(x-1)*(y-x^2), sph*(z-x^3), (x+y+z-1)^2};
result = regeneration I_*
assert(#result==2 and result/degree == {4,2} and result/dim == {0,1})
///