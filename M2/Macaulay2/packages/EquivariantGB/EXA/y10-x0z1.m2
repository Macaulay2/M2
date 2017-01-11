restart
needsPackage "EquivariantGB"
-- QQ[x_0,x_1,...; z_0,z_1,...; y_(0,1),y(1,0),...]
R = buildERing({symbol x, symbol z, symbol y}, {1,1,2}, QQ, 2, MonomialOrder=>Lex, Degrees=>{1,1,2});
gens R
F = {y_(1,0) - x_0*z_1}; -- finishes
F = {y_(1,0) - x_0*z_1,y_(0,1) - x_1*z_0};
egbSignature(F, PrincipalSyzygies=>false)
