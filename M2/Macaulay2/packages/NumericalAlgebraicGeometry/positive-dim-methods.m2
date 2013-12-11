------------------------------------------------------
-- routines for positive-dimensional solution sets 
-- not included in other files
-- (loaded by  ../NumericalAlgebraicGeometry.m2)
------------------------------------------------------

numericalVarietyM2 = I -> numericalVariety flatten (regeneration I_* / decompose)
numericalVarietyBertini = I -> bertiniPosDimSolve I_*
numericalVariety Ideal := I -> (
     if DEFAULT.Software === BERTINI 
     then numericalVarietyBertini 
     else numericalVarietyM2
     ) I

isOn = method(Options=>{Tolerance=>null,Software=>null})
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
isOn (Point,NumericalVariety) := o -> (p, V) -> (
    o = fillInDefaultOptions o;
    if o.Software === BERTINI then #bertiniComponentMemberTest(V,{p})>0
    else error "not implemented"    
    )

TEST ///
R = CC[x,y]	
I = ideal((x^2+y^2+2)*x,(x^2+y^2+2)*y);
W = witnessSet(ideal I_0 , ideal(x-y), {
	point {{ 0.999999*ii,0.999999*ii}}, 
	point {{ -1.000001*ii,-1.000001*ii}}
	} )
assert isOn(point {{sqrt 5*ii,sqrt 3}},W)
assert not isOn(point {{sqrt 5*ii,1.7}},W)
///

