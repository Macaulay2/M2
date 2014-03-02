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
isOn (Point,WitnessSet) := o -> (p, V) -> (
    o = fillInDefaultOptions o;
    --if o.Software === BERTINI then bertiniComponentMemberTest(numericalVariety {V},{p})
    --else 
    error "not implemented"    
    )
isOn (Point,NumericalVariety) := o -> (p, V) -> (
    o = fillInDefaultOptions o;
    if o.Software === BERTINI then #bertiniComponentMemberTest(V,{p})>0
    else error "not implemented"    
    )
