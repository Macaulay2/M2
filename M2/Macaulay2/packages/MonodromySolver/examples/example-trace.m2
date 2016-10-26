needsPackage "MonodromySolver"
setRandomSeed 0
m = 2; n = 4; d = 3;  
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
G = polySystem(F|L)
end

restart
load "example-trace.m2"
(p0, x0) := createSeedPair(G);
elapsedTime (V,npaths) = monodromySolve(G,p0,{x0},NumberOfEdges=>4,Verbose=>true)
length V.PartialSols

-- collect system and solutions obtained 
G = V.Graph
sys = polySystem specializeSystem(V.BasePoint, G.Family)
sols = points V.PartialSols

-- INPUT: list of points OUTPUT: coordinatewise sum as column vector
computeTrace = L -> sum apply(L, t -> matrix t)

-- pick 3 generic points on 
params = gens coefficientRing ring G.Family
lastB = last params
traces = {transpose(computeTrace sols | matrix {{last coordinates p0}})}
linearSlice = apply(flatten entries G.Family.PolyMap, F -> sub(sub(F, ring G.Family), toList apply(0..(length params -2), i -> params#i => (p0.Coordinates)#i)))
for i from 0 to 2 do (
    b = random(RR);
    sys' = polySystem apply(linearSlice, F->sub(F, lastB => b));
    T = track(sys, sys', sols);
    print (T/matrix/transpose , transpose computeTrace T);
    traces = append(traces, transpose (computeTrace T | matrix{{b}}))
    );

(traces#0 | traces#1 | traces#2)
(t1,t2,t3) = (traces#0- traces#1, traces#0- traces#2, traces#1-traces#2)
first SVD(traces#2-traces#1|traces#3-traces#1)


-- b = apply(specializeSystem(p0, G.Family), F -> sub(F,apply(gens ring F, g-> g=> 0)))

{* TRACE TEST needed!

(1) create a graph that includes three nodes that correspond to generic slices in an affine linear one-parameter family
    (e.g., fix all parameters except b_1; pick random 3 values for b_1) 

(2) the sum of the solutions for these 3 nodes will behave linearly (as a function of b_1) iff...

(3) ... the 3 solution sets are complete

(4) check that by making a 2-by-(n+1) matrix which is rank-deficient iff (2)  

*}
