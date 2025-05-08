restart
needsPackage "NAGtools"
--setDefault(Software=>M2)
setRandomSeed 0
needsPackage "ExampleIdeals"
-- n = 10
--time degree cyclicRoots(n,ZZ/32003)
F = gens cyclicRoots(n,CC)
R = ring F
X = apply(gens R, v->inputGate (symbol x)_v) -- variables
monoms = flatten entries monomials F
toGate = m -> product apply(X,first first listForm m,(x,a)->x^a)
M = hashTable apply(monoms, m->m=>toGate m) -- monomials
polys = flatten entries F
C = apply(#polys,i-> -- parametric coefficients 
    apply(flatten entries monomials polys#i, m->inputGate (symbol c)_(i,m))
    )
S = transpose matrix{
    apply(#polys,i->sum(numcols monomials polys#i, j->
	    C#i#j*M#((monomials polys#i)_(0,j))
	    ))
    }
-*
F = transpose matrix{flatten C}
PH = gateHomotopy4preimage(F,S,X|flatten C,flatten C)
x0 = apply(X,x->1)
c0 = point{ 
    flatten apply(C,r->(
	    t := apply(#r-1, i->random CC);
	    t | { -sum t }
	    )) 
    }
pre0 = point{x0|coordinates c0}
*-
PH = parametricSegmentHomotopy(S,X,flatten C)
x0 = apply(X,x->1)
c0 = point{ 
    flatten apply(C,r->(
	    t := apply(#r-1, i->random CC);
	    t | { -sum t }
	    )) 
    }
pre0 = point{x0}

end ----------------------------------------------------------------------------

restart
n = 7
load "NumericalAlgebraicGeometry/SYSTEMS/monodromy/sparse-system.m2"
needsPackage "PHCpack"
debug NumericalAlgebraicGeometry
phcF = toRingXphc flatten entries F
mv =  mixedVolume(phcF,StartSystem => false)
stop = (n,L)->#L>=mv
elapsedTime pre'all = preimageViaMonodromy(PH,c0,{pre0},
    StoppingCriterion=>stop);
-- RHEL:  

---------- PHCpack timing ------------------------
restart
needsPackage "PHCpack"

needsPackage "ExampleIdeals"
n = 7
I = cyclicRoots(n,CC);
R = CC[x_1..x_(numgens ring I)]
toR = map(R,ring I,vars R)
elapsedTime mixedVolume(I_*/toR,StartSystem => false)
elapsedTime (mv,q,qsols) = mixedVolume(I_*/toR,StartSystem => true);

