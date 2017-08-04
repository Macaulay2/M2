-* 
  M2 < y10-x0x0x1-PS-false.m2 > y10-x0x0x1-PS-false.out
*- 

needsPackage "EquivariantGB"
-- QQ[x_0,x_1,...; y_(0,1),y(1,0),...]
R = buildERing({symbol x, symbol y}, {1,2}, QQ, 2, MonomialOrder=>Lex, Degrees=>{1,2});
gens R
F = {y_(1,0) - x_0^2*x_1};
-- kernel of y_(i,j) -> x_i*x_j with i > j
egbSignature(F, PrincipalSyzygies=>false)

end

restart
