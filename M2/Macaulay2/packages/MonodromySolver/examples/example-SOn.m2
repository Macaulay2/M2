needsPackage "MonodromySolver"
LIEDIM=binomial(N,2)--This is the dimension of SO(N)
R=CC[c_1..c_LIEDIM,t_(1,1,1)..t_(LIEDIM,N,N)][x_(1,1)..x_(N,N)]--here c_i are the constants in the linear form indexed by i.
--and t_(i,j,k) is the coefficient of x_(j,k) in the linear form indexed by i
M=genericMatrix(R,N,N)--next three lines create equations for SO(N)
B=M*transpose(M)-id_(R^N)
polys=flatten for j from 0 to N-1 list for k from j to N-1 list B_(j,k)--Here they are
--below gives linear slices where the coefficients {c_i,t_(i,j,k)} form the parameter space
linearSlice=for i from 1 to LIEDIM list(c_i+sum(flatten for j from 1 to N list  for k from 1 to N list t_(i,j,k)*x_(j,k))); --this takes a random cut
G = polySystem join (polys,linearSlice)--the incidence variety.
end

restart
N=6;
--where N is as in SO(N)
load "example-SOn.m2"
describe R
setRandomSeed 0 
x0coords = flatten entries id_(CC^N)
(p0, x0) := createSeedPair(G,x0coords);
elapsedTime (V,npaths) = monodromySolve(G,p0,{x0},NumberOfEdges=>4,Verbose=>true)
length V.PartialSols
