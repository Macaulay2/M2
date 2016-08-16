needsPackage "MonodromySolver"
setRandomSeed 0
needsPackage "ExampleIdeals"

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
n = 10
load "../examples/cyclic.m2" 
{*
load "~/R/polysparse/examples/cyclic.m2" 
*}

debug MonodromySolver
mixedVolume = computeMixedVolume polys

plugin'c0 = map(R,AR,vars R | matrix c0) -- the actual polynomial system we solve
apply(polysP,p->plugin'c0 p) 
stop = (n,L)->n>1
getDefault Software
{*
setDefault(Software=>PHCPACK)
*}
setRandomSeed 0

elapsedTime sols = solveViaMonodromy(SP,c0,{pre0},StoppingCriterion=>stop);
{*
number of paths tracked: 151542
found 11016 points in the fiber so far
     -- 688.773 seconds elapsed
*}

setRandomSeed 0
nedges = 4
elapsedTime sols' = monodromySolve(SP,c0,{pre0},NumberOfEdges=>nedges,TargetSolutionCount=>mixedVolume)
{*
     -- 181.88 seconds elapsed

o21 = (HomotopyGraph{...4...}, 44064)
*}
