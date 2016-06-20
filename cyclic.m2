needs "../code/solveViaMonodromy.m2"
setRandomSeed 0
needsPackage "ExampleIdeals"
n = 6
S = gens cyclicRoots(n,CC)
R = ring S
polys = flatten entries S
ind = flatten apply(#polys,i-> -- parameteric coefficients 
    apply(exponents polys#i, t->(i,t))
    )
AR = CC[apply(ind,i->A_i)][gens R] 
polysP = for i to #polys-1 list -- parameteric coefficients 
         sum(exponents polys#i, t->A_(i,t)*AR_(t))
SP = matrix{polysP}

c0 = point{ 
    flatten apply(polys,f->(
	    r := # exponents f;
	    t := apply(r-1, i->random CC);
	    t | { -sum t }
	    )) 
    }
pre0 = point{toList(n:1_CC)}
end ------------------------------------------------

restart
load "cyclic.m2" 
stop = (n,L)->n>1
getDefault Software
{*
setDefault(Software=>PHCPACK)
*}
elapsedTime sols = solveViaMonodromy(SP,c0,{pre0},StoppingCriterion=>stop);
