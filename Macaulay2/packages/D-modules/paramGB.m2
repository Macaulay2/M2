--------------------------------------------------------------
-- Computes parametric GB
--------------------------------------------------------------

--some helpful funcs
ring(List) := g -> ring g#0
gens (List) := g -> matrix{g#0}
getChangeMat = method()
getChangeMat (List) := g->g#1
makeMonic := f -> (  
     l := leadCoefficient f;
     reduceF ((1 / l) * f)  
     );

myLCM = method();
myLCM(List) := l -> (
     if #l == 0 then null
     else ( 
	  I := ideal l#0;
	  i := 1;
	  while i<#l do (
	       I = intersect(I, ideal l#i); i = i + 1
	       );
	  first first entries mingens I
	  )
     );

reduceF = method();
reduceF(RingElement) := f -> (
     R := ring f;
     if isField R then (numerator f)/(denominator f)
     else if (isPolynomialRing R) 
     then sum(listForm f / (u->(
			 --print ring u#1;
			 reduceF(u#1) * R_(u#0)
			 )
			 ))     	 
     else f
     );
reduceF(List) := l -> (l / (u -> reduceF u));
reduceF(QQ) := f -> f;

in = f -> if f!=0 then leadTerm f else 0_(ring f) 

isdivisible = (f,g) -> (
     if (f==0) or (g==0) 
     then false 
     else(
     	  a := listForm f;
       	  b := listForm g;	
       	  all(a#0#0-b#0#0, u->(u>=0))
     	  )
     )

---------------------------------------------------------------
-- paramGB
-- input: I: ideal
--        ChangeMatrix=>false: option
-- output: { Groebner basis generators for I [, changematrix] }

paramGB = method(Options => {ChangeMatrix=>true})
paramGB(Thing) := (o-> I -> (
    if class I === Ideal then
    I = gens I;
    R := ring I;
    WAflag := R.monoid.Options.WeylAlgebra =!= {};
    if WAflag then (
	 createDpairs R;
	 xVar := R.dpairInds#0;
	 dxVar := R.dpairInds#1;
	 );
    K := last (coefficientRing R).baseRings;
    isQuotFlag := isQuotientRing(K);  
    L := if isQuotFlag then ring presentation K else K;
    varP := first entries vars L;
    np := #varP;
    ord := R.monoid.Options.MonomialOrder;
    newMonomialOrder := ord -> (
	 if class ord === Nothing then ProductOrder {numgens R, np}
	 else if class ord === Eliminate then 
	 ProductOrder {ord#0, numgens R - ord#0, np}
	 else if class ord === ProductOrder then
	 ProductOrder ((toList ord) | {np}) 
	 );
    Rgens = R.generatorSymbols;
    Lgens = L.generatorSymbols;
    
    -- make a new ring with parameters
    NewR := (coefficientRing L) [
	 xxx_0..xxx_(numgens R - 1),
	 mmm_0..mmm_(np-1),  
	 WeylAlgebra => 
	 (if WAflag 
	      then apply(#xVar, i -> (xxx_(xVar#i) => xxx_(dxVar#i)))
	      else {}
	      ),
	 --WeylAlgebra => R.monoid.Options.WeylAlgebra--, 
	 --MonomialSize => 16,
	 MonomialOrder => newMonomialOrder R.monoid.Options.MonomialOrder
	 ];
    L2NewR := map( NewR, L, toList(0..np-1) / (i->mmm_i) );
    Zero := if isQuotFlag then L2NewR ideal presentation K
    else ideal 0_NewR;
    R2NewR := f -> (
	 l := listForm f;
	 mlcm := myLCM (l / (u -> lift(denominator u#1, L)));
	 l = l / (u -> ( u#0, lift(numerator u#1, L) * (
			mlcm // lift(denominator u#1, L)) ));
	 sum(l, (u -> (L2NewR u#1) * NewR_( toList u#0 | toList(np : 0) )))
	 );
    J := first entries I / R2NewR;
    g := first entries gens gb (ideal J + Zero);
    
    -- go back to the original ring
    NewR2R := f -> (
	 l := listForm f;
	 l = l / (u -> 
	      (drop(toList u#0, -np), u#1 * L_(drop(toList u#0, numgens R))));
	 sum(l, (u -> promote(u#1, K) * R_(u#0)))    
	 );
    g = g / NewR2R;      	  
    
    -- clean up
    i := 0;
    while i < #g do (
	 if g#i == 0_R
	 then (
	      --print ("dropping", g#i); 
	      g = drop(g, {i,i})
	      )
	 else i = i + 1; 
	 );
    i = 0;
    while i < #g do (
	 if any(drop(g, {i,i}), u->isdivisible(in g#i, in u)) 
	 then g = drop(g, {i,i})
	 else i = i + 1; 
	 );
    
    -- construct the matrix of invertibles
    inv := matrix { g / (f -> (
		   (1_K / (leadCoefficient f))*1_R
		   ))};
    
    -- make every generator monic
    g = g / ( u -> makeMonic u);
    
    {g, inv}
    ));









