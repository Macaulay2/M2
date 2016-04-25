restart
needsPackage "NAGtools"
--setDefault(Software=>M2)
setRandomSeed 0
needsPackage "ExampleIdeals"
n = 10
--time degree cyclicRoots(n,ZZ/32003)
S = gens cyclicRoots(n,CC)
R = ring S
X = apply(gens R, v->inputGate (symbol x)_v) -- variables
monoms = flatten entries monomials S
toGate = m -> product apply(X,first first listForm m,(x,a)->x^a)
M = hashTable apply(monoms, m->m=>toGate m) -- monomials
polys = flatten entries S
C = apply(#polys,i-> -- parameteric coefficients 
    apply(flatten entries monomials polys#i, m->inputGate (symbol c)_(i,m))
    )
S = transpose matrix{
    apply(#polys,i->sum(numcols monomials polys#i, j->
	    C#i#j*M#((monomials polys#i)_(0,j))
	    ))
    }
{*
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
*}
PH = parametricSegmentHomotopy(S,X,flatten C)
x0 = apply(X,x->1)
c0 = point{ 
    flatten apply(C,r->(
	    t := apply(#r-1, i->random CC);
	    t | { -sum t }
	    )) 
    }
pre0 = point{x0}
stop = (n,L)->n>5
--nextP = () -> point {{ c0 }}
pre'all = preimageViaMonodromy(PH,c0,{pre0},
    --RandomPointFunction=>nextP,
    StoppingCriterion=>stop);

end
restart
load "sparse-system.m2"


---------- PHCpack timing ------------------------
restart
loadPackage "PHCpack"

needsPackage "ExampleIdeals"
n = 10
I = cyclicRoots(n,CC);
R = CC[x_1..x_(numgens ring I)]
toR = map(R,ring I,vars R)
elapsedTime (mv,q,qsols) = mixedVolume(I_*/toR,StartSystem => true);

