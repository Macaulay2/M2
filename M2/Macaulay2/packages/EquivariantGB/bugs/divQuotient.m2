-*
this bug is produced by running y10-x0x0x1-PS-false.m2 for... one hour 
*-
restart
debug needsPackage "EquivariantGB"
R = buildERing({symbol x, symbol y}, {1,2}, QQ, 5, MonomialOrder=>Lex, Degrees=>{1,2});
gp = x_2*y_(1,0)-x_1*y_(2,0)
jp = x_4*x_2*y_(3,0)*y_(2,1)*y_(2,0)-x_4*x_0^4*y_(3,2)*y_(2,1)
(isDiv,Q) = divQuotient(gp,jp)

leadTerm(jp)
leadTerm(Q*gp)
