-* TODO
1) bihomogenize
2) divide-conquer summation for trace matrix?
3) more parameters
4) better abstraction
*-

restart
load "example-CRN.m2"


-- toy network example in comments
-*(p0, x0) = createSeedPair(G,"initial parameters" => "one")
elapsedTime (V,npaths) = monodromySolve(G,p0,{x0},
    GraphInitFunction=>completeGraphInit,
    NumberOfEdges=>5,
    "new tracking routine"=>false,
    Verbose=>true)
assert(length V.PartialSols ==4)*-

setRandomSeed 0
W = wnt()
L = apply(numgens createRing(W,FF), i->random FF)
F = createPolySystem(W, FF, L)
(p0, x0) = createSeedPair(F,L)
elapsedTime (V,npaths) = monodromySolve(F,p0,{x0},
    GraphInitFunction=>completeGraphInit,
    NumberOfEdges=>5,
--    TargetSolutionCount => 9,
    "new tracking routine"=>false,
    Verbose=>true)
assert(length V.PartialSols == 9)

Gr = V.Graph
W1 = apply(toList points (first Gr.Vertices).PartialSols, p -> matrix {p0.Coordinates|p.Coordinates})

R = ring Gr.Family
S = coefficientRing R

-*
in T ring, let's have generators for xhyperplane and khyperplanes and also maybe homogenize equations
*-

T =  CC[a, apply(numgens R, i -> (symbol WW)_i)][gens S, gens R]
mSysEqs = apply(flatten entries Gr.Family.PolyMap, i-> sub(i,T))

-- generate a random point on the solution variety
mSys = polySystem mSysEqs;
svcodim = (mSys.NumberOfVariables - mSys.NumberOfPolys)
assert(svcodim == numgens S and svcodim == #coordinates p0)

-- make sure p0 lies on all khyperplanes
khyperplanes0' = (sub(vars S,T) - matrix p0) * random(CC^svcodim,CC^(svcodim));
khyperplanes0 = submatrix'(khyperplanes0',,{0});
-- ... and (x0,a0) satisfies the following. 
xcoeffs = random(CC^(numgens R),CC^1);



xhyperplane = (sub(vars R, T) * transpose submatrix'(vars coefficientRing T,,{0}))_(0,0) - a;
seedCoeffs = (matrix x0 * xcoeffs)_(0,0)
a0 = {seedCoeffs}

P' = polySystem transpose (matrix{mSysEqs} | khyperplanes0  | xhyperplane)

-- second Wntness set: max I've gotten for Wnt model is 256
elapsedTime (V',npaths) = monodromySolve(P', point{a0|flatten entries xcoeffs}, {point{coordinates p0 | coordinates x0}}, 
     NumberOfNodes => 6, NumberOfEdges => 8, "new tracking routine" => false, GraphInitFunction=>completeGraphInit, Verbose=>true, EdgesSaturated=>true)

W = last V'.Graph.Vertices
xhyperplane0 = first specializeSystem(W.BasePoint, polySystem {xhyperplane})
U = ring xhyperplane0
W2 = apply(points W.PartialSols, p -> matrix p)

-- combine witness sets for trace test
sols = apply(W1 | W2, s -> point s);

curve = sub(matrix{mSysEqs}|khyperplanes0,U) 
Pquadric = polySystem transpose (curve|matrix{{xhyperplane0 * sub(first flatten entries khyperplanes0',U)}});
(t1,t2) = (random CC, random CC)
targetHyperplane = random(1,U)

Plinear = polySystem transpose (curve|matrix{{targetHyperplane+1}}) 
Plinear1 = polySystem transpose (curve|matrix{{targetHyperplane+t1}}) 
Plinear2 = polySystem transpose (curve|matrix{{targetHyperplane+t2}}) 

-* tests for Pqadric
min apply(sols, s->norm evaluate(Pquadric,s))
apply(sols, s->norm evaluate(curve,s))
apply(sols, s->norm evaluate(matrix{{xhyperplane0}},s))
apply(sols, s->norm evaluate(matrix{{sub(first flatten entries khyperplanes0',U)}},s))
apply(sols, s->numericalRank evaluate(jacobian Pquadric,s))
*-

tracked = track(Pquadric,Plinear,sols);
tracked1 = track(Plinear, Plinear1, tracked);
tracked2 = track(Plinear1, Plinear2, tracked1);
computeTrace = L -> sum apply(L, t -> matrix t)
traces = apply({tracked, tracked1, tracked2}, x -> transpose computeTrace x)
first SVD ((traces#0-traces#1) | (traces#0-traces#2))





-*m = 2; n = 4; d = 3;  
C = CC[a_(1,1)..a_(m,n),b_1..b_m]
R = C[x_1..x_n]
F = apply(n-m, i->sub(random(d,CC[x_1..x_n]),R)) -- V(F) = intersection of n-m hypersurfaces of degree d
A = genericMatrix(C,n,m)
B = genericMatrix(C,b_1,1,m)
L = flatten entries (vars R * A + B) -- slice of complimentary dimension 
G = polySystem(F|L)

 clearAll()
C = CC[b]
R = C[x,y,z]
F = {y-x^2, z-x^3} 
L = { 2*x + 3*y + 5*z - b }
G = polySystem(F|L) *-
end

