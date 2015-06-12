restart
loadPackage "EquivariantGB"

-- QQ[x_0,x_1,...]
R = buildERing({symbol x}, {1}, QQ, 2);
gens R
F = {x_0 + x_1};
egb(F)

-- QQ[x_0,x_1,...; y_(0,1),y(1,0),...]
R = buildERing({symbol x, symbol y}, {1,2}, QQ, 2, MonomialOrder=>Lex, Degrees=>{1,2});
gens R
F = {y_(1,0) - x_0*x_1};
-- kernel of y_(i,j) -> x_i*x_j with i > j
G1 = egb(F, OutFile=>stdio)
G2 = egbSignature F
break
G3 = egbSignature(F, PrincipalSyzygies=>true)
selectInSubring(1,matrix{G1})

F = {y_(1,0) - x_0*x_1^2, y_(0,1) - x_0^2*x_1};

R = buildERing({symbol y,symbol x},{2,1},QQ,3, Diagonal=>false)
S = buildERing({symbol x},{1},QQ,3, Diagonal=>false)
M = buildEMonomialMap(S, R, {x_0*x_1, x_0})
G = egbToric M

restart
loadPackage "EquivariantGB"
debug EquivariantGB
I = shift{0,2,3}
J = shift{1,2,3}
I*J
J*I
divWitness(I,J)
divWitness(J,I)
K = shift{1,2,5}
divWitness(I,K)
divWitness(J,K)

I = shift{}
J = shift{0,2}
divWitness(I,J)

R = buildERing({symbol x}, {1}, QQ, 4);
S = shiftMonomial(x_2,shift{1})
T = shiftMonomial(x_3,shift{1})
divWitness(S,T)


restart
loadPackage "EquivariantGB"
R = buildERing({symbol x},{0},QQ,1)
F = {x^2+1, x-1}
egbSignature F

restart
loadPackage "EquivariantGB"
R = buildERing({symbol x, symbol y}, {1,2}, QQ, 2, MonomialOrder=>Lex);
F = {y_(1,0) - x_0*x_1};
egbSignature F
egb(F, OutFile=>stdio)
///
                                                       2
o10 = {x x  - y   , x y    - x y   , x y    - x y   , x y    - y   y   , y   y    - y   y   , y   y    - y   y   }
        1 0    1,0   2 1,0    0 2,1   1 2,0    0 2,1   0 2,1    2,0 1,0   3,2 1,0    3,0 2,1   3,1 2,0    3,0 2,1
///

R = buildERing({symbol y},{1},QQ,2)
F = {y_0 + y_1}
egbSignature F


restart 
needsPackage "ExampleIdeals"
needsPackage "EquivariantGB" 
R = buildERing(toList(a..d),{0,0,0,0},QQ,1)
describe R
I = cyclicRoots(4,QQ)
F = (sub(I,R))_*
eGB = egbSignature F
jGB = first entries gens gb ideal F
eGB / leadMonomial // sort
jGB / leadMonomial // sort

R = buildERing(toList(a..e),{0,0,0,0,0},ZZ/101,1)
I = katsura(5,QQ)
