needsPackage "MonodromySolver"
setRandomSeed 0
needsPackage "ExampleIdeals"

n= 7;

S = gens cyclicRoots(n,CC)
R = ring S
polys = flatten entries S
ind = flatten apply(#polys,i-> -- parametric coefficients 
    apply(exponents polys#i, t->(i,t))
    )
AR = CC[apply(ind,i->A_i)][gens R] 
polysP = for i to #polys-1 list -- parametric coefficients 
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

mixedVolume = computeMixedVolume polys
numSeeds = 20
result = for i from 0 to numSeeds - 1 list (
  setRandomSeed i;
  (dynamicFlowerSolve(SP,c0,{pre0},TargetSolutionCount=>mixedVolume))#1)
  
<< sum(result)/numSeeds;
end ------------------------------------------------
