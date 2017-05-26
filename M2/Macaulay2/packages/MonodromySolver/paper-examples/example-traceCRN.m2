{*
This file walks through a basic implementation of the trace test for verifying root counts. For terminology, please see "Trace Test" by
Leykin, Rodriguez, Sotille. 
*}

restart
load "./sol-count-smaller-than-bkk-examples/example-CRN.m2"
setRandomSeed 0

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
W1 = apply(toList points (first Gr.Vertices).PartialSols, p -> matrix {p0.Coordinates|p.Coordinates})

-- W1 is a partial witness collection for a curve C, obtained by slicing the solution variety with khyperplanes below
R = ring Gr.Family
S = coefficientRing R
T =  CC[a, apply(numgens R, i -> (symbol WW)_i)][gens S, gens R]
mSysEqs = apply(flatten entries Gr.Family.PolyMap, i-> sub(i,T))
mSys = polySystem mSysEqs;
svcodim = (mSys.NumberOfVariables - mSys.NumberOfPolys)
assert(svcodim == numgens S and svcodim == #coordinates p0)
khyperplanes0' = (sub(vars S,T) - matrix p0) * random(CC^svcodim,CC^(svcodim));
khyperplanes0 = submatrix'(khyperplanes0',,{0});

-- to apply the multihomogeneous trace test, another partial witness set for the curve must be found
xcoeffs = random(CC^(numgens R),CC^1);
xhyperplane = (sub(vars R, T) * transpose submatrix'(vars coefficientRing T,,{0}))_(0,0) - a;
seedCoeffs = (matrix x0 * xcoeffs)_(0,0)
a0 = {seedCoeffs}
P' = polySystem transpose (matrix{mSysEqs} | khyperplanes0  | xhyperplane)
elapsedTime (V',npaths) = monodromySolve(P', point{a0|flatten entries xcoeffs}, {point{coordinates p0 | coordinates x0}}, 
     NumberOfNodes => 3, NumberOfEdges => 3, GraphInitFunction=>completeGraphInit)
assert(length V'.PartialSols == 11)

-- W2 is a partial witness collection of multidegree 11. together, W1 and W2 form a multihomogeneous witness set for C
W = last V'.Graph.Vertices
xhyperplane0 = first specializeSystem(W.BasePoint, polySystem {xhyperplane})
U = ring xhyperplane0
W2 = apply(points W.PartialSols, p -> matrix p)
sols = apply(W1 | W2, s -> point s);

-- the witness set (W1,W2) tracks to the hyperplane section Plinear in a suitable affine chart for C
curve = sub(matrix{mSysEqs}|khyperplanes0,U) 
Pquadric = polySystem transpose (curve|matrix{{xhyperplane0 * sub(first flatten entries khyperplanes0',U)}});
(t1,t2) = (random CC, random CC)
targetHyperplane = random(1,U)
Plinear = polySystem transpose (curve|matrix{{targetHyperplane+1}}) 

-- by solving parallel translates of Plinear, we obtain 3 test points (tracked, tracked1, tracked2) for verifying linearity
Plinear1 = polySystem transpose (curve|matrix{{targetHyperplane+t1}}) 
Plinear2 = polySystem transpose (curve|matrix{{targetHyperplane+t2}}) 
tracked = track(Pquadric,Plinear,sols);
tracked1 = track(Plinear, Plinear1, tracked);
tracked2 = track(Plinear1, Plinear2, tracked1);

{*linearity of the trace at these test points is equivalent to rank-deficiency of the 11 x 2 matrix constructed below:
a small singular value may be regarded as evidence that (W0,W1) is a complete witness set 
*}
computeTrace = L -> sum apply(L, t -> matrix t)
traces = apply({tracked, tracked1, tracked2}, x -> transpose computeTrace x)
min first SVD ((traces#0-traces#1) | (traces#0-traces#2)) 



