restart
loadPackage "EquivariantGB"

-- QQ[x_0,x_1,...]
R = buildERing({symbol x}, {1}, QQ, 2);
gens R
F = {x_0 + x_1};
egb(F)

-- QQ[x_0,x_1,...; y_(0,1),y(1,0),...]
R = buildERing({symbol x, symbol y}, {1,2}, QQ, 2, MonomialOrder=>Lex);
gens R
F = {y_(1,0) - x_0*x_1};
-- kernel of y_(i,j) -> x_i*x_j with i > j
G1 = egb(F, OutFile=>stdio)
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

R = buildERing({symbol x, symbol y}, {1,2}, QQ, 2, MonomialOrder=>Lex);
F = {y_(1,0) - x_0*x_1};
egbSignature F
egb(F, OutFile=>stdio)

R = buildERing({symbol y},{1},QQ,2)
F = {y_0 + y_1}
egbSignature F
