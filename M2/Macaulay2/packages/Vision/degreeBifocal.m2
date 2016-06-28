restart
needsPackage "NAGtools"
FF = CC_53
R = FF[b_(2,1)..b_(3,3),c_2,c_3]
S = FF[f_(1,1)..f_(3,3)]
-- A = transpose genericMatrix(R,a_(1,1),3,3)
A = id_(R^3)
B = matrix{{0_R,0,0}}||transpose genericMatrix(R,b_(2,1),3,2) 
C = matrix{{1},{c_2},{c_3}}
O = matrix{{0_R},{0},{0}}
M = (A|O)||(B|C)
L1 = reverse subsets(3,2) 
L2 = reverse subsets(3,2) + toList(3:{3,3})
matrix apply(3, i -> apply(3, j -> det M^(L1#i|L2#j)))
phi = map(R, S, flatten apply(3, i -> apply(3, j -> det M^(L1#i|L2#j))))
-- determinant of fundamental matrix
ker phi
F = transpose genericMatrix(S,f_(1,1),3,3)
det F
ideal det F == ker phi
v0 = random(FF^1,FF^(numgens R))
phi
b0 = sub(matrix phi, v0)
RP = FF[k_1..k_(numgens S)][gens R, s]
SP = sub(matrix phi,RP) - s*sub(random(FF^1,FF^(numgens S)),RP) + b0 + vars coefficientRing RP
elapsedTime sols = solveViaMonodromy(SP, point map(FF^1, FF^(numgens S), 0), {point (v0|matrix{{0}})})
#sols                                    
VerticalList sols
imageSols = apply(sols, s->sub(matrix phi, matrix {drop(coordinates s, -1) }))
fold(imageSols, (a,b)->a||b)
first SVD oo
