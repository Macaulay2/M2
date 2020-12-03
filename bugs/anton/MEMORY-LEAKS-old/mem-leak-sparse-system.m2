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

GHS = PH.GateHomotopySystem
EH = GHS#"EH"
mc0 = matrix c0;
mpre0 = matrix pre0;
inp = mutableMatrix(mc0 | mc0 | mpre0 | matrix{{.5+.3*ii}})
retH = GHS#("retH",CC_53)
cc = .5+.3*ii


mc0 = matrix c0
mpre0 = matrix(CC,{coordinates pre0})
time elapsedTime for i to 10000 do ( 
    M = mc0 | mc0 | mpre0;
    inp = mutableMatrix (M | matrix {{ .3+.4*ii}});
    -- evaluate(EH,inp,retH)
    )
-- 8 sec


mc0 = matrix c0;
mpre0 = matrix pre0;
mc0 = mutableMatrix matrix c0;
mpre0 = mutableMatrix matrix pre0;
fillIn = M -> (
    i := 0;
    scan(numcols mc0, c->(M_(0,i)=mc0_(0,c);i=i+1));
    scan(numcols mc0, c->(M_(0,i)=mc0_(0,c);i=i+1));
    scan(numcols mpre0, c->(M_(0,i)=mpre0_(0,c);i=i+1));
    M_(0,i)=cc
    )  
e1 = first entries mc0;
e2 = first entries mpre0;
fillIn2 = M -> (
    i := 0;
    scan(numcols mc0, c->(M_(0,i)=e1#c;i=i+1));
    scan(numcols mc0, c->(M_(0,i)=e1#c;i=i+1));
    scan(numcols mpre0, c->(M_(0,i)=e2#c;i=i+1));
    M_(0,i)=cc
    )  
fillIn3 = M -> (
    i := 0;
    for c in e1 do (M_(0,i)=c;i=i+1);
    for c in e1 do (M_(0,i)=c;i=i+1);
    for c in e2 do (M_(0,i)=c;i=i+1);
    M_(0,i)=cc
    )  
time elapsedTime for i to 20000 do ( 
    fillIn3 inp;
    evaluate(EH,inp,retH);
    )
-- 5 sec for both Matrix/MutableMatrix 

elapsedTime for i to 100000 do ( 
    evaluate(EH,inp,retH);
    )

-- 0.19 sec (370 times speedup)
-- with fillMatrix inp: 1.5 sec (45 times speedup)	    
