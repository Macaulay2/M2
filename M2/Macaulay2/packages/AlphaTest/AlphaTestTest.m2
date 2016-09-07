needs "~/polysparse/code/MonodromySolver/solveViaMonodromy.m2"
setRandomSeed 0
needs "~/M2/M2/Macaulay2/packages/ExampleIdeals.m2"
n = 6
S = gens cyclicRoots(n,FF)
R = ring S
polys = flatten entries S
ind = flatten apply(#polys,i-> -- parameteric coefficients 
    apply(exponents polys#i, t->(i,t))
    )
AR = FF[apply(ind,i->A_i)][gens R] 
polysP = for i to #polys-1 list -- parameteric coefficients 
         sum(exponents polys#i, t->A_(i,t)*AR_(t))
SP = matrix{polysP}

c0 = point{ 
    flatten apply(polys,f->(
	    r := # exponents f;
	    t := apply(r-1, i-> random CC);
	    t | { -sum t }
	    )) 
    }
pre0 = point{toList(n:1_FF)}
end ------------------------------------------------

restart
needsPackage "AlphaTest"
FF = CC
load "AlphaTest/AlphaTestTest.m2"
needs "~/polysparse/code/MonodromySolver/solveViaMonodromy.m2"
plugin'c0 = map(R,AR,vars R | matrix c0) -- the actual polynomial system we solve
apply(polysP,p->plugin'c0 p) 
stop = (n,L)->n>1
getDefault Software
{*
setDefault(Software=>PHCPACK)
*}
elapsedTime sols = solveViaMonodromy(SP,c0,{pre0},StoppingCriterion=>stop);
FFF = QQ[k]/ideal(k^2+1)
complexToRational((coordinates(sols #0)),FFF)
SPP = polySystem(transpose plugin'c0 SP)
all ( sols, s -> certifySolutions(SPP,s))
pp=point{apply( coordinates((sols)#0), s -> complexToRational(s,FFF))}
certifySolutions(SPP,pp)
certifySolutions(SPP, (sols)#0)
computeConstants(SPP, (sols)#0)
computeConstants(SPP,pp)
SPP' = complexToRational(SPP, FFF)
sols' = complexToRational(sols,FFF)
all (sols', s -> certifySolutions(SPP', s))
apply(sols', s -> computeConstants(SPP', s))






