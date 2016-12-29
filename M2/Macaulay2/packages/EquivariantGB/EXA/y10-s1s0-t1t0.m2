restart
needsPackage "EquivariantGB"
-- QQ[s_0,s_1,...; t_0,t_1,...; y_(0,1),y(1,0),...]
R = buildERing({symbol s, symbol t, symbol y}, {1,1,2}, QQ, 2, MonomialOrder=>Lex, Degrees=>{1,1,2});
gens R
F = {y_(1,0) - s_0*s_1 - t_0*t_1}; 
egb(F, Algorithm=>Signature, OutFile=>stdio)
