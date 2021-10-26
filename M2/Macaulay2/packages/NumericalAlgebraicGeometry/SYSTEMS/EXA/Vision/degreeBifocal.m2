restart
needsPackage "NAGtools"
FF = CC_53
R = FF[b_(2,1)..b_(3,3),c_2,c_3]
S = FF[f_(1,1)..f_(3,3)]
-- A = transpose genericMatrix(R,a_(1,1),3,3)
A = id_(R^3)
B = matrix{{0_R,0,0}}||transpose genericMatrix(R,b_(2,1),3,2)
C = matrix{{1},{c_2},{c_3}} -- center of the 2nd camera
O = matrix{{0_R},{0},{0}}
M = (A|O)||(B|C) -- 1st and 2nd cameras stacked
L1 = reverse subsets(3,2) 
L2 = reverse subsets(3,2) + toList(3:{3,3})
phi = map(R, S, flatten apply(3, i -> apply(3, j -> (-1)^(i+j)*det M^(L1#i|L2#j))))
-- determinant of fundamental matrix
ker phi
F = transpose genericMatrix(S,f_(1,1),3,3)
ideal det F == ker phi

setRandomSeed 0
AB0 = random(FF^1,FF^(numgens R))
F0 = sub(matrix phi, AB0)

X1 = random(FF^4,FF^1) -- pick a world point
x1 = sub((A|O)*X1,FF)
y1 = sub(B|C,AB0)*X1
F0'33 = matrix(F0,3,3)
assert(clean_0.00001 (transpose x1 * F0'33 * y1) == 0)
L1 = transpose x1 * F * y1
N1 = sub(last coefficients L1,FF)

m = numgens S-1
RP = FF[k_1..k_m][gens R, s]
dir = numericalKernel(transpose N1,0.00000001)*(random(FF^m, FF^1))
assert(clean_0.00001 (transpose dir*N1) == 0)
dir' = numericalKernel(transpose N1,0.00000001)*transpose vars coefficientRing RP
SP = sub(matrix phi,RP) - s*sub(transpose dir,RP) - (F0 + transpose dir')

stop = (n,L)->n>10
elapsedTime sols = solveViaMonodromy(SP, point map(FF^1, FF^m, 0), {point (AB0|matrix{{0}})}, StoppingCriterion=>stop)

#sols                                    
VerticalList sols
imageSols = apply(sols, s->sub(matrix phi, matrix {drop(coordinates s, -1) }))
fold(imageSols, (a,b)->a||b)
first SVD oo
