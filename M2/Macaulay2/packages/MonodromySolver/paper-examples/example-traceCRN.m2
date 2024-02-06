-*
This file walks through a basic implementation of the trace test for verifying root counts. For terminology, please see "Trace Test" by
Leykin, Rodriguez, Sotille. 
*-

restart
load "./sol-count-smaller-than-bkk-examples/example-CRN.m2"

setRandomSeed 0
W = wnt()
G=createPolySystem(W,CC)


-- finds 9 steady--state solutions for the Wnt model
(p0, x0) = createSeedPair(G,"initial parameters" => "one")
elapsedTime (V,npaths) = monodromySolve(G,p0,{x0},
    GraphInitFunction=>completeGraphInit,
    NumberOfEdges=>4,
    NumberOfNodes=>3,
    "new tracking routine"=>false,
    Verbose=>true)
assert(length V.PartialSols ==9)
Gr = V.Graph
W1 = apply(points V.PartialSols, p -> matrix {p0.Coordinates|p.Coordinates})


-- W1 is a partial witness collection for a curve C, 
-- ??? obtained by slicing the solution variety with khyperplanes below ???
R = ring Gr.Family
S = coefficientRing R
svcodim=numgens S
sliceInds = apply((numgens S+1)*(numgens S-1),ind-> (symbol VV)_ind) | apply(numgens R, i -> (symbol WW)_i)
T =  CC[sliceInds|{b}][gens S, gens R]
fakeParams = transpose vars coefficientRing T
trueParams=(vars T)_(toList(0..numgens S-1))|matrix{{1_T}}
trueVars=(vars T)_(toList(numgens S..numgens S + numgens R -1))

L=apply(numgens S-1,i->(trueParams*fakeParams^(toList(i*(numgens S+1)..(i+1)*(numgens S+1)-1)))_(0,0))
xSlice=(trueVars*fakeParams^(toList((numgens S)^2 -1..(numgens S)^2 +numgens R-2)))_(0,0)+b
L=L|{xSlice}

mSysEqs = apply(flatten entries Gr.Family.PolyMap, i-> sub(i,T))
mSys = polySystem(mSysEqs|L);

assert(mSys.NumberOfVariables == mSys.NumberOfPolys) -- checking this is a square system

bigBase=point{p0.Coordinates|x0.Coordinates}
subTable=apply(gens T,bigBase.Coordinates,(g,c)->g=>c)
seedEqs = apply(equations mSys,e->sub(e,subTable));
lineqs=polySystem apply(take(seedEqs,{numgens R, numgens R+svcodim-1}),p->sub(p,T))
p=first createSeedPair polySystem apply(take(seedEqs,{numgens R, numgens R+svcodim}),p->sub(p,T))

setRandomSeed 0
elapsedTime (V',npaths)=monodromySolve(mSys,p,{bigBase},"new tracking routine"=>false,Verbose=>true)

--???  W2 is a partial witness collection of multidegree 11. ???
-- together, W1 and W2 form a multihomogeneous witness set for C
W = last V'.Graph.Vertices
xhyperplane0 = first specializeSystem(W.BasePoint, polySystem {xSlice});
W2 = apply(points W.PartialSols, p -> matrix p);
sols = apply(W1 | W2, s -> point s);

end ----------------------------------------
restart
load "./example-traceCRN.m2"

khyperplanes0'=specializeSystem(W.BasePoint,polySystem take(equations mSys,{numgens R, numgens R+svcodim-2}))

-- the witness set (W1,W2) tracks to the hyperplane section Plinear in a suitable affine chart for C
U=ring xhyperplane0
curve = sub(matrix{mSysEqs|drop(L,-1)},U);
Pquadric = polySystem transpose (curve|matrix{{xhyperplane0 * sub(first khyperplanes0',U)}});
(t1,t2) = (random CC, random CC);
targetHyperplane = random(1,U);
Plinear = polySystem transpose (curve|matrix{{targetHyperplane+1}});


-- by solving parallel translates of Plinear, we obtain 3 test points (tracked, tracked1, tracked2) for verifying linearity
Plinear1 = polySystem transpose (curve|matrix{{targetHyperplane+t1}});
Plinear2 = polySystem transpose (curve|matrix{{targetHyperplane+t2}});
tracked = track(Pquadric,Plinear,sols);
tracked1 = track(Plinear, Plinear1, tracked);
tracked2 = track(Plinear1, Plinear2, tracked1);

-*linearity of the trace at these test points is equivalent to rank-deficiency of the 11 x 2 matrix constructed below:
a small singular value may be regarded as evidence that (W0,W1) is a complete witness set 
*-
computeTrace = L -> sum apply(L, t -> matrix t)
traces = apply({tracked, tracked1, tracked2}, x -> transpose computeTrace x)
first SVD ((traces#0-traces#1) | (traces#0-traces#2)) 

-** thoughts on what TO DO **
Make stopping via linear trace test an option for 
solveFamily

(start with a standalone routine that runs the trace test given W1 and W2)   

By the way, solveFamily should 
(1) check if the system is indeed linear in parameters
(2) implement a dynamic strategy (expanding the graph if necessary)
(3) fix documentation: 
 Description
       ===========

       The output of "monodromySolve" is "technical." This method is intended for
       users uninterested in the underlying "HomotopyGraph" and its satellite
       data.
       
Also, see ??? for the comments above -- fix those.
*-

