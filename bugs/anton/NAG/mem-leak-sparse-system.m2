needsPackage "NAGtools"
--setDefault(Software=>M2)
setRandomSeed 0
needsPackage "ExampleIdeals"
n = 7
n = 5 --      -- 174. seconds elapsed
--degree cyclicRoots(n,ZZ/32003)
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
PH = parametricSegmentHomotopy(S,X,flatten C)
x0 = apply(X,x->1)
c0 = point{ 
    flatten apply(C,r->(
	    t := apply(#r-1, i->random CC);
	    t | { -sum t }
	    )) 
    }
pre0 = point{x0}
p0 = transpose matrix c0
p1 = random(CC^(#coordinates c0),CC^1)

{*
SPH = specialize(PH,p0||p1)
for i to 1000000 do (
    pre1 = trackHomotopy(SPH,{pre0});
    if i%10 == 0 then (
	<< "<<<<<"  << i << endl;
	print (first pre1).NumberOfSteps
    );
    )
*}
NAGtrace 2
elapsedTime preimageViaMonodromy(PH,c0,{pre0},StoppingCriterion=>((n,L)->n>0));

end
restart
load "../../../bugs/anton/NAG/mem-leak-sparse-system.m2"

elapsedTime for i to 100000 do 
evaluateH(
    PH.GateHomotopySystem
    , 
    transpose(matrix c0 | matrix c0 | matrix pre0), .5+.3*ii)
-- approx. 70 sec... and it does not depend on n.  
GHS = PH.GateHomotopySystem
GHS#(GHS#"H",CC_53)
