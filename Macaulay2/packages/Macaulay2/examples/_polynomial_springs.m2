R = QQ[x,y,z];
R
describe R
0_R
1_R
11_R
R_0^10+R_1^3+R_2
numgens R
apply(numgens R, i -> R_i^i)
sum(numgens R, i -> R_i^i)
gens R
vars R
index x, index y, index z
coefficientRing R
random(2,R)
basis(2,R)
ZZ[a,b,c][d,e,f];
(a+d+1)^2
QQ[rho,sigma,tau];
(rho - sigma)^2
ZZ[b..k];
ZZ[symbol b .. symbol k];
vars (0..4)
ZZ[vars (0..4),vars(26..30),vars 51]
ZZ[t,p_0,p_1,q_0,q_1];
ZZ[p_(0,0) .. p_(2,1),q_0..q_5]
(p_(0,0)+q_2-1)^2
protect xx; protect yy; protect zz;
ZZ[ee_[xx],ee_[yy],ee_[zz]]
ZZ[a,b,c] === ZZ[a,b,c]
