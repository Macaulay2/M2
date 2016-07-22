-------------------------------------------------------
-- functions related to numerical primary decomposition
-- (loaded by  ../NumericalAlgebraicGeometry.m2)
-------------------------------------------------------
export { "isPointEmbedded", "isPointEmbeddedInCurve", "AllVisible", "Regularity" }

debug NumericalHilbert

isPointEmbedded = method(Options=>{AllVisible=>false})
isPointEmbedded(Point, Ideal, List) := o -> (p,I,C) -> ( -- C is a list of witness sets for irreducible components
    R := ring I;
    time gCs := gCorners(p,I); -- assume Robert's
	       	              -- algorithm a DualSpace D,
			      -- and g-corners
    d := 0;
    l := random(1,ring I); -- a generic linear form 
    while true do (
	-- FIRST PART: returns true if embeddedness is certified
	if o.AllVisible then (
    	    Jd := interpolate(p,I,C,d);  -- a function that returns 
	    -- a list of polynomials forming a basis of J_d (vector
	    -- space), where J is the "part" of the decomposition of I
	    -- corresponding to the components given in C.
	    if dim R_d - dim Jd != hilbertFunction(d, monomialIdeal gCs) then return true; -- is there a better way?
	    )
	else (
	    if DBG>0 then print "-- double truncation...";
	    time Jdd := doubleTruncation(I,C,d,d);
	    if DBG>0 then print (d,dim Jdd);
	    for d' from 1 to d do (
		g := random(d', Jdd);
		if DBG>0 then << "-- witness poly: (d',d) = " << (d',d) << endl;
		time if isWitnessPolynomial(p,I,g,d)     
		then (
		    if DBG>0 then print toString g;
		    return true;
		    )
		)
	    );
        --SECOND PART: returns false if deemed not embedded
	if DBG>0 then print "-- colon(truncated dual)...";
	time Sd := colon(truncatedDual(p,I,d), l);
	sCs := flatten entries sCorners gCs;
	colonLMs := leadMonomial \ flatten entries gens reduceSpace Sd;
    	if DBG>0 then << "-- s-corners: " << sCs << endl << "-- LM(dual of colon ideal): " << colonLMs << endl;	
	if isSubset(sCs, colonLMs) then return false; 
    	d = d+1;
	)
    )

interpolate = method()
interpolate (Point,Ideal,List,ZZ) := (p,I,C,d) -> error "not implemented"

TEST ///
restart
needs "npd.m2"
needsPackage "NumericalAlgebraicGeometry"
-- NPD2.8: pseudo-component at the origin
RQQ = QQ[x_1..x_3]
M = matrix{{x_1^2,x_1*x_2*x_3}}

-- NPD3.10: all components are embedded
RQQ = QQ[x_1..x_3]
M = matrix{{x_1^2,x_1*x_2^2*x_3,x_1*x_2*x_3^3}}

I = ideal M
RCC = CC[x_1..x_3]
C = drop(flatten flatten (ass I / values@@numericalIrreducibleDecomposition),-1)
O = origin RCC
isPointEmbedded(O,sub(I,RCC),C)
///

doubleTruncation = method()
doubleTruncation (Ideal,List,ZZ,ZZ) := (I,C,d,e) -> (
    R := ring I;
    S := polySpace basis(0,d,R);
    orthogonalInSubspace(I,C,e,S)
    ) 

orthogonalInSubspace (Ideal,List,ZZ,PolySpace) := (I,C,e,S) -> (
    t := 1e-6;
    done := false;
    while not done do done = all(C, V->(
	    p := random V; 
	    D := truncatedDual(p,I,e); -- D_p^e[I]
	    S' := orthogonalInSubspace(D,S,t);
	    nothing'new := (dim S' == dim S);
	    S = S';
	    nothing'new
	    ));
    S
    )

isWitnessPolynomial = method()
isWitnessPolynomial (Point, Ideal, RingElement, ZZ) := (p,I,g,dStop) -> (
    t := 1e-6;
    R := ring g;
    n := numgens R;
    if g == 0 then return false;
    Igens := sub(gens I, matrix{gens R + apply(p.Coordinates,c->sub(c,R))});
    g = sub(g, matrix{gens R + apply(p.Coordinates,c->sub(c,R))});
    e := first degree g;
    
    Iplusg := Igens|matrix{{g}};
    IP := truncDualData(Igens,false,t);
    IplusgP := truncDualData(Iplusg,false,t);
    IhP := truncDualData(Igens,true,t);
    ID := IhD := IplusgD := polySpace map(R^1,R^0,0);
    S := ring homogPolySpace IhP;
    dehomog := map(R,S,{1_R} | gens R);
    gh := homogenize(sub(g,S),S_0);
    d := 0;
    GCs := {};
    varList := new MutableList from n:false;
    ginI := true;
    ginJ := false;
    while d <= dStop do (
	IP = nextTDD(d,IP,t);
	IplusgP = nextTDD(d,IplusgP,t);
	if dim polySpace IP != dim polySpace IplusgP then ginI = false;
	IhP = nextTDD(d+e,IhP,t);
	IhD = dualSpace(homogPolySpace IhP, point {toList (numgens S:0)});
	IhcolonghD := reduceSpace polySpace dehomog gens colon(IhD,gh);
	newGCs := newGCorners(IhcolonghD,GCs,d,d);
	if any(newGCs, g->(g#0 == 1_R)) then return false;
	for g in newGCs do (
	    l := (listForm(g#0))#0#0;
	    ls := select(n, i->(l#i != 0));
	    if #ls == 1 then varList#(first ls) = true;
	);
	if all(varList, v->v) then ginJ = true;
      	GCs = GCs|newGCs;
	d = d+1;
	if ginJ and not ginI then return true;
	);
    false
    )

TEST ///
restart
needs "npd.m2"
R = CC[x,y]
p = point {{0,0}}
I = ideal {x^2,y*x}
g = x^2
assert(not isWitnessPolynomial(p,I,g,10))
h = x
assert(isWitnessPolynomial(p,I,h,10))

-- NPD2.8: pseudo-component at the origin
RQQ = QQ[x_1..x_3]
M = matrix{{x_1^2,x_1*x_2*x_3}}

-- NPD3.10: all components are embedded
RQQ = QQ[x_1..x_3]
M = matrix{{x_1^2,x_1*x_2^2*x_3,x_1*x_2*x_3^3}}
///


isPointEmbeddedInCurve = method(Options=>{Regularity=>-1})
isPointEmbeddedInCurve (Point,Ideal) := o-> (p,I) -> (
    R := ring I;
    I' := ideal sub(gens I, matrix{gens R + apply(p.Coordinates,c->sub(c,R))});
    p' := origin(R);
    m := matrix{apply(gens R, v->random(1,R))}; -- matrix for random linear change of coordinates
    I' = (map(R,R,m)) I'; -- I with new coordinates
    r := o.Regularity;
    if r == -1 then (
	r1 := localHilbertRegularity(p',I');
	r2 := dim truncatedDual(p',I',r);
	r = max{r1,r2-1};
	);
    E := eliminatingDual(p',I',{0},r); -- assume I is in general position w.r.t. x = R_0
    E1 := truncate(E,{0},r-1);
    E2 := colon(E,R_0);
    print (dim E1, dim E2, areEqual(E1,E2));
    dim E1 != dim E2 -- "truncate" extracts a lesser truncated eliminating dual from E
    -- can we make a less expensive test than above? look at the leading terms of the dual basis (and the quotient)?
    )