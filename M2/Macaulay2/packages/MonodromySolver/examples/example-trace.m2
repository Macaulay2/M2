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
end

restart
load "example-trace.m2"
(p0, x0) := createSeedPair(G);
elapsedTime (V,npaths) = monodromySolve(G,p0,{x0},NumberOfEdges=>4,Verbose=>true)
length V.PartialSols

{* TRACE TEST needed!

(1) create a graph that includes three nodes that correspond to generic slices in an affine linear one-parameter family
    (e.g., fix all parameters except b_1; pick random 3 values for b_1) 

(2) the sum of the solutions for these 3 nodes will behave linearly (as a function of b_1) iff...

(3) ... the 3 solution sets are complete

(4) check that by making a 3-by-(n+1) matrix which is rank-deficient iff (2)  

*}
