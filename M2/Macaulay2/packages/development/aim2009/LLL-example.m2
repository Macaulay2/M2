restart
nbits = 200;
nbits = 300;
rr = RR_nbits;

-- example 1: affine twisted cubic
R = rr[a,b,c]
p = numeric(nbits,pi)
F = map(rr,R,{p, p^2, p^3})
--viewHelp basis
row1 = F(basis(0,2,R))
L = flatten entries(2^(nbits-8) * row1)
L = L/floor
row1 = matrix{L}
M = row1 || (id_(source row1))
M1 = LLL M
M1 = submatrix(M1, 1..10, {0,1,2})
basis(0,2,R) * M1

-- example 2: secant locus to the veronese in P^5
R = rr[a,b,c,d,e,f]
p1 = log numeric_nbits 5
p2 = log numeric_nbits 7
p3 = log numeric_nbits 11
p4 = log numeric_nbits 13
p5 = log numeric_nbits 17
F = map(rr,R,{p1^2,p1*p2,p2^2,p1,p2,1})
G = map(rr,R,{p3^2,p3*p4,p4^2,p3,p4,1})
P1 = flatten entries (p5 * F.matrix + G.matrix)
H = map(rr,R,P1)
D = 3
L = (flatten entries (H basis(D,R)))/(x -> floor(2^(nbits-8) * x))
L = matrix{L}
M = L || id_(source L);
M0 = LLL M
MTM = (transpose M0) * M0
for i from 0 to numColumns MTM-1 list sqrt(MTM_(i,i))
sort oo

M1 = submatrix(M0, 1..(numRows M-1), {0})
RQQ = QQ[gens R]
basis(D,RQQ) * M1
det genericSymmetricMatrix(RQQ,3)
