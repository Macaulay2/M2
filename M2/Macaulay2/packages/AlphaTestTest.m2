-- first, third lines depend  on where polysparse is ocated on your machine
needs "~/Summer_Projects/polysparse/code/solveViaMonodromy.m2"
setRandomSeed 0
needs "~/Summer_Projects/M2/M2/Macaulay2/packages/ExampleIdeals.m2"
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
load "AlphaTestTest.m2"
needs "~/Summer_Projects/polysparse/code/solveViaMonodromy.m2"
plugin'c0 = map(R,AR,vars R | matrix c0) -- the actual polynomial system we solve
apply(polysP,p->plugin'c0 p) 
stop = (n,L)->n>1
getDefault Software
{*
setDefault(Software=>PHCPACK)
*}
elapsedTime sols = solveViaMonodromy(SP,c0,{pre0},StoppingCriterion=>stop);

sols = solveViaMonodromy(SP,c0,{pre0},StoppingCriterion=>stop)
SPP = polySystem(transpose plugin'c0 SP)
all ( sols, s -> certifySolutions(SPP,s))

FFF = QQ[k]/ideal(k^2+1)

complexToRational((coordinates(sols #0)),FFF)

peek SPP
M = (coefficients (flatten entries SPP.PolyMap)#0)#0
pluginR' = map(R', R, vars R)
pluginR' M_(0,0)
sub((coefficients M_(0,0))#0, R')

sub(M_(0,0), R')
R' = FFF[gens ring (flatten entries SPP.PolyMap)#0]
gens R'
methods(sub)
 = 
methods substitute

substitute(SPP, FFF)

pp=point{apply( coordinates((sols)#0), s -> complexToRational(s,FFF))}
certifySolutions(SPP,pp)
certifySolutions(SPP, (sols)#0)
computeConstants(SPP, (sols)#0)
computeConstants(SPP,pp)
complexToRational(plugin'c0 SP, FFF)
