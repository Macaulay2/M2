R = ZZ[a..d];
a*d + b*c
R = ZZ[a..d, MonomialOrder => {Lex => 4}];
1+a+b+c+d+a*d+b*c
R = ZZ[a..d,MonomialOrder => {Weights => {1,0,0,1}}];
1+a+b+c+d+a*d+b*c
R = ZZ[a..d,MonomialOrder=>GRevLex]
