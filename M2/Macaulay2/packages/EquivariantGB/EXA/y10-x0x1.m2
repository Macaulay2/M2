needsPackage "EquivariantGB"
-- QQ[x_0,x_1,...; y_(0,1),y(1,0),...]
R = buildERing({symbol x, symbol y}, {1,2}, QQ, 2, MonomialOrder=>Lex, Degrees=>{1,2});
gens R
F = {y_(1,0) - x_0*x_1};
-- kernel of y_(i,j) -> x_i*x_j with i > j
elapsedTime egb(F, OutFile => currentFileDirectory|"y10-x0x1.Buchberger.out")
elapsedTime egb(F, Algorithm => Incremental, OutFile => currentFileDirectory|"y10-x0x1.Incremental.out")
elapsedTime egb(F, Algorithm => Signature, PrincipalSyzygies=>false, OutFile => currentFileDirectory|"y10-x0x1.Signature-PS-false.out")
elapsedTime egb(F, Algorithm => Signature, PrincipalSyzygies=>true, OutFile => currentFileDirectory|"y10-x0x1.Signature-PS-true.out")
-* 
  To see the difference in output for Algorithm=>Signature: 
    diff y10-x0x1.Signature-PS-false.out y10-x0x1.Signature-PS-true.out > y10-x0x1.diff
*- 
end

-- start with a GB
restart
needsPackage "EquivariantGB"
-- QQ[x_0,x_1,...; y_(0,1),y(1,0),...]
R = buildERing({symbol x, symbol y}, {1,2}, QQ, 4, MonomialOrder=>Lex, Degrees=>{1,2});
F = {y_(1,0) - x_0*x_1,  x_2*y_(1,0)-x_0*y_(2,1), x_1*y_(2,0)-x_0*y_(2,1), x_0^2*y_(2,1)-y_(2,0)*y_(1,0), y_(3,2)*y_(1,0)-y_(3,0)*y_(2,1), y_(3,1)*y_(2,0)-y_(3,0)*y_(2,1)};
-- kernel of y_(i,j) -> x_i*x_j with i > j
egbSignature(F, PrincipalSyzygies=>false)
