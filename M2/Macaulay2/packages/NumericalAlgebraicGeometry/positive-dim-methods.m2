------------------------------------------------------
-- routines for positive-dimensional solution sets 
-- not included in other files
-- (loaded by  ../NumericalAlgebraicGeometry.m2)
------------------------------------------------------

isOn (AbstractPoint,NumericalVariety) := o -> (p, V) -> (
    o = fillInDefaultOptions o;
    if o.Software === BERTINI then #bertiniComponentMemberTest({p},V)>0
    else any(keys V, d->any(V#d, C->isOn(p,C,o)))  
    )

TEST ///
R = CC[x,y]	
e = 0.0000001
V = numericalIrreducibleDecomposition ideal (x*(x-1), x*(y-1))
assert isOn( point{{e,random CC}}, V ) 
assert isOn( point{{1-e,1+e}}, V ) 
assert not isOn( point{{1,0}}, V ) 
///

