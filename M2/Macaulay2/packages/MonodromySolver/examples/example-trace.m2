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

-- affine trace test
G = V.Graph
sys = polySystem specializeSystem(V.BasePoint, G.Family)
sols = points V.PartialSols
computeTrace = L -> sum apply(L, t -> matrix t)
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
first SVD(traces#2-traces#1|traces#3-traces#1)
