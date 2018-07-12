-*
This file walks through a basic implementation of the trace test for verifying root counts. For terminology, please see "Trace Test" by
Leykin, Rodriguez, Sotille. 
*-

restart
load "./sol-count-smaller-than-bkk-examples/example-CRN.m2"
setRandomSeed 0

W = wnt()
G=createPolySystem(W,CC)

(n,m)=(numgens ring G,numgens coefficientRing ring G)

-- finds 9 steady--state solutions for the Wnt model
(p0, x0) = createSeedPair(G,"initial parameters" => "one")
elapsedTime (V,npaths) = monodromySolve(G,p0,{x0},
    GraphInitFunction=>completeGraphInit,
    NumberOfEdges=>4,
    NumberOfNodes=>2,
    "new tracking routine"=>false,
    Verbose=>true)
assert(length V.PartialSols ==4)
Gr = V.Graph
W1 = apply(points V.PartialSols, p -> matrix {V.BasePoint.Coordinates|p.Coordinates})


-- W1 is a partial witness collection for a curve C, obtained by slicing the solution variety with khyperplanes below
R = ring Gr.Family
S = coefficientRing R
svcodim=numgens S
sliceInds = apply((numgens S+1)*(numgens S-1),ind-> (symbol VV)_ind) | apply(numgens R, i -> (symbol WW)_i)
T =  CC[sliceInds|{b}][gens S, gens R]
fakeParams = transpose vars coefficientRing T
trueParams=(vars T)_(toList(0..numgens S-1))|matrix{{1_T}}
trueVars=(vars T)_(toList(numgens S..numgens S + numgens R -1))

--pKernel=numericalKernel(matrix{V.BasePoint.Coordinates},10^-6)

L=apply(numgens S-1,i->(trueParams*fakeParams^(toList(i*(numgens S+1)..(i+1)*(numgens S+1)-1)))_(0,0));
xSlice=(trueVars*fakeParams^(toList((numgens S)^2 -1..(numgens S)^2 +numgens R-2)))_(0,0)+b
L=L|{xSlice}

mSysEqs = apply(flatten entries Gr.Family.PolyMap, i-> sub(i,T))
mSys = polySystem(mSysEqs|L);
(mSys.NumberOfVariables , mSys.NumberOfPolys)

bigBase=point{p0.Coordinates|x0.Coordinates}
subTable=apply(gens T,bigBase.Coordinates,(g,c)->g=>c)
seedEqs = apply(equations mSys,e->sub(e,subTable));
lineqs=polySystem apply(take(seedEqs,{numgens R, numgens R+svcodim-1}),p->sub(p,T))
p=first createSeedPair polySystem apply(take(seedEqs,{numgens R, numgens R+svcodim}),p->sub(p,T))

setRandomSeed 0
(numE,numV)=(2,3)
elapsedTime (V',npaths')=monodromySolve(mSys,p,{bigBase},
    "new tracking routine"=>false,Verbose=>true)--,NumberOfNodes=>numV,NumberOfEdges=>numE,
    --TargetSolutionCount=>300,SelectEdgeAndDirection=>selectBestEdgeAndDirection,Potential=>potentialLowerBound,NumberOfRepeats=>numE*numV)
assert(length V'.PartialSols == 271)


W = last V'.Graph.Vertices
Pwitness=specializeSystem(W.BasePoint,mSys);


pHyperplanes=apply(svcodim,i->( 
	pHyperplaneMatrix:=(sub(trueParams,ring first Pwitness) * random(CC^(m+1),CC^1));
 	(pHyperplaneMatrix-evaluate(pHyperplaneMatrix,first W1))_(0,0)
	));

xhyperplane0=last Pwitness;
PtraceWitness=take(Pwitness,{0,n-1}) | drop(pHyperplanes,-1) | {xhyperplane0};


W2=track(Pwitness,PtraceWitness,points W.PartialSols);


--PP=specializeSystem(W.BasePoint,matrix{mSysEqs|drop(pHyperplanes,-1)|{xSlice}})
--track(specializeSystem(W.BasePoint,mSys),PP,points W.PartialSols)


--end



--khyperplanes0'=specializeSystem(W.BasePoint,polySystem take(equations mSys,{numgens R, numgens R+svcodim-2}))

-- the witness set (W1,W2) tracks to the hyperplane section Plinear in a suitable affine chart for C


refine(PtraceWitness,W2);


sols = apply(W1 | W2, s -> point s);
--#W1
--#sols

U=ring xhyperplane0
curve = drop(PtraceWitness,-1);
Pquadric=curve|{(last pHyperplanes ) * xhyperplane0};
--max apply(sols,x->norm evaluate(polySystem Pquadric,x))


--polySystem({curve | {(last pHyerplanes ) * xhyperplane0}})

--curve = sub(matrix{mSysEqs|drop(L,-1)},U);
--Pquadric = polySystem transpose (curve|matrix{{xhyperplane0 * sub(last pHyperplanes,U)}});
(t1,t2) = (random CC, random CC);
targetHyperplane = random(1,U);
Plinear = curve|{targetHyperplane+1};


-- by solving parallel translates of Plinear, we obtain 3 test points (tracked, tracked1, tracked2) for verifying linearity
Plinear1 = curve|{targetHyperplane+t1};
Plinear2 = curve|{targetHyperplane+t2};


tracked = track(Pquadric,Plinear,sols);
refine(Plinear,tracked);
tracked1 = track(Plinear, Plinear1, tracked);
refine(Plinear1,tracked1);
tracked2 = track(Plinear1, Plinear2, tracked1);
refine(Plinear2,tracked2);
checkPrecision=max apply(tracked2,x->norm evaluate(polySystem Plinear2,x)); -- this should be small

computeTrace = L -> sum apply(L, t -> matrix t);
traces = apply({tracked, tracked1, tracked2}, x -> transpose computeTrace x);
first SVD ((traces#0-traces#1) | (traces#0-traces#2));
