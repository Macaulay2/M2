A = QQ[x,y,z,MonomialOrder=>Lex];
I = ideal(x^2+y+z-1, x+y^2+z-1, x+y+z^2-1);
transpose gens gb I
