------------------------------------------------------
-- numerical intersection routines 
-- (loaded by  ../NumericalAlgebraicGeometry.m2)
------------------------------------------------------

export {"hypersurfaceSection", "numericalIntersection"}

insertComponent = method()
insertComponent(WitnessSet,MutableHashTable) := (W,H) -> (
     d := dim W;
     --if H#?d then H#d#(#H) = W -- ??? 
     if H#?d then H#d#(#(H#d)) = W 
     else H#d = new MutableHashTable from {0=>W};
     )

isPointOnAnyComponent = method()
isPointOnAnyComponent(Point,HashTable) := (p,H) -> any(keys H, d -> any(keys H#d, k -> isOn(p,H#d#k)))

splitWitness = method(TypicalValue=>Sequence, Options =>{Tolerance=>null})
splitWitness (WitnessSet,RingElement) := Sequence => o -> (w,f) -> (
-- splits the witness set into two parts: one contained in {f=0}, the other not
-- IN:  comp = a witness set
--      f = a polynomial
-- OUT: (w1,w2) = two witness sets   
     o = fillInDefaultOptions o;
     w1 := {}; w2 := {};
     for x in w#Points do 
	 if residual(matrix {{f}}, matrix x) < o.Tolerance 
	 then w1 = w1 | {x}
	 else w2 = w2 | {x};   
     ( if #w1===0 then null else witnessSet(ideal equations w + ideal f, 
	     -* this is "stretching" the convention that this has to be a complete intersection *-
	     w.Slice, w1), 
       if #w2===0 then null else witnessSet(w.Equations, w.Slice, w2) 
       )
   )

hypersurfaceSection = method(Options =>{Software=>null, Output=>Singular})
hypersurfaceSection(NumericalVariety,RingElement) := o -> (c1,f) -> (
    if DBG>1 then << "-- hypersurfaceSection: "<< endl << 
    "NumericalVariety: " << c1 << endl <<
    "hypersurface: " << f << endl;
    d := sum degree f;
    R := ring f;
    if getDefault Normalize then f = f/sqrt(numgens R * BombieriWeylNormSquared f); 
    c2 := new MutableHashTable; -- new components
    for comp in components c1 do (
	if DBG>2 then << "*** proccesing component " << peek comp << endl;
	(cIn,cOut) := splitWitness(comp,f); 
	if cIn =!= null then (
	    if DBG>2 then << "( regeneration: " << net cIn << " is contained in V(f) for" << endl <<  
	    << "  f = " << f << " )" << endl;
	    scan(
		partitionViaDeflationSequence( cIn.Points, 
		    polySystem(equations polySystem cIn | slice cIn) 
		    -- this is overdetermined!
		    ),
		pts -> (
		    oldW := witnessSet(cIn.Equations, cIn.Slice, pts);
		    if DBG>2 then << "   old component " << peek oldW << endl;
		    check oldW;    
		    insertComponent(oldW,c2);
		    )
		);
--	    insertComponent(cIn,c2)
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
	    local'regular'seq := equations polySystem comp;
	    

	    S := polySystem( local'regular'seq
		| { product flatten apply( dWS, w->sliceEquations(w.Slice^{0},R) ) } -- product of linear factors
		| sliceEquations(slice',R) );
	    T := polySystem( local'regular'seq
		| {f}
		| sliceEquations(slice',R) );
	    
	    -- deflate if singular
	    P := first comp.Points;
	    if status P =!= Singular then targetPoints := track(S,T,flatten apply(dWS,points), 
		NumericalAlgebraicGeometry$gamma=>exp(random(0.,2*pi)*ii),
		Software=>o.Software)
    	    else (
	 	seq := P.DeflationSequenceMatrices;
	 	S' := squareUp(deflate(S, seq), squareUpMatrix P.LiftedSystem); -- square-up using the same matrix
		T' := squareUp(deflate(T, seq), squareUpMatrix P.LiftedSystem); -- square-up using the same matrix
		S'sols := flatten apply(dWS,W->apply(W.Points,p->p.LiftedPoint));
		
	     	T'.PolyMap = (map(ring S', ring T', vars ring S')) T'.PolyMap; -- hack!!!: rewrite with trackHomotopy
	     	lifted'w' := track(S',T',S'sols, 
		    NumericalAlgebraicGeometry$gamma=>exp(random(0.,2*pi)*ii), Software=>o.Software);
	     	targetPoints = apply(lifted'w', p->(
		     	q := project(p,T.NumberOfVariables);
		     	q.System = T;
		     	q.LiftedSystem = T';
		     	q.LiftedPoint = p;
		     	q.SolutionStatus = Singular;
		     	q
		     	));
	 	);
	    LARGE := 100; ---!!!
	    refinedPoints := refine(T, targetPoints, 
		ErrorTolerance=>DEFAULT.ErrorTolerance*LARGE,
		ResidualTolerance=>DEFAULT.ResidualTolerance*LARGE,
		Software=>o.Software);
	    regPoints := select(refinedPoints, p->p.SolutionStatus===Regular);
	    singPoints := select(refinedPoints, p->p.SolutionStatus===Singular);
	    targetPoints = if o.Output == Regular then regPoints else regPoints | solutionsWithMultiplicity singPoints;
	    if DBG>2 then << "( regeneration: " << net cOut << " meets V(f) at " 
	    << #targetPoints << " points for" << endl 
	    << "  f = " << f << " )" << endl;
	    f' := ideal (equations comp | {f});
	    nonJunkPoints := select(targetPoints, p-> not isPointOnAnyComponent(p,c2)); -- this is very slow		    
	    scan(partitionViaDeflationSequence(nonJunkPoints,T),
		pts -> (
		    newW := witnessSet(f',slice',selectUnique(pts, Tolerance=>1e-4));--!!!
		    if DBG>2 then << "   new component " << peek newW << endl;
		    check newW;    
		    insertComponent(newW,c2);
		    )
		)
	    ) 
	);
    	numericalVariety flatten apply(keys c2, i->apply(keys c2#i, j->c2#i#j))
    )


numericalIntersection = method()
numericalIntersection (NumericalVariety, Ideal) := (V,I) -> (
    W := new NumericalVariety from V;
    for f in I_* do W = hypersurfaceSection(W,f);
    W
    )
numericalIntersection (WitnessSet,WitnessSet) := (W1,W2) -> (
    R := ring W1;
    if R =!= ring W2
    then error "expected witness sets in the same ambient space";
    y := symbol y;
    n := numgens R;
    S := (coefficientRing R) monoid([gens R, y_1..y_n]);
    x := take(gens S,n);
    y = take(gens S,-n);
    toSx := map(S,R,x);
    toSy := map(S,R,y);
    W12 := witnessSet(toSx ideal equations W1 + toSy ideal equations W2, 
	toSx ideal slice W1 + toSy ideal slice W2,
	flatten apply(W1.Points, P1->apply(W2.Points, P2-> 
		point {coordinates P1 | coordinates P2}
		))
	);
    numericalIntersection(
	numericalVariety {moveSlice(W12,randomSlice(dim W12, numgens ring W12, coefficientRing ring W12))}, 
	ideal apply(n, i->x#i-y#i)
	)
    )
numericalIntersection (NumericalVariety,NumericalVariety) := (V1,V2) -> (
    V := numericalVariety {};
    for W1 in components V1 do
    for W2 in components V2 do V = V | numericalIntersection(W1,W2);
    V 
    )
TEST ///
CC[x,y,z]; 
W1 = witnessSet (ideal (x^2+y), 2)
W2 = witnessSet (ideal (x^2+y^2-1), 2)
V = numericalIntersection(W1,W2)
assert(dim V == 1 and degree V == 4)
///
