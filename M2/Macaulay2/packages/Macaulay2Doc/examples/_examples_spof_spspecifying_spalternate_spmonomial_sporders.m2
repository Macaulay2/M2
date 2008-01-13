R = ZZ[a..d];
a+b^100+c*d
R = ZZ[a..d, MonomialOrder=>Lex];
a+b^100+c*d
R = ZZ[a..d, MonomialOrder => Weights => {201,2}];
a+b^100+c*d
R = ZZ[a..d, MonomialOrder=>{Weights=>4:1,Lex}];
a+b^100+c*d
R = ZZ[a..f, MonomialOrder=>Eliminate 2];
a+b^100+c*d
R = ZZ[a..f, MonomialOrder=>Weights=>2:1];
a+b^100+c*d
R = ZZ[a..f, MonomialOrder=>{2,4}];
a^2*(c+d) + b*(c^100+d^100)*(c + e + f)
R = ZZ[a..f, MonomialOrder=>{Weights=>2:1,Lex}]
a^2*(c+d) + b*(c^100+d^100)*(c + e + f)
R = ZZ[a..f, MonomialOrder => GroupLex => 3];
a^-2*(c+d) + b*(c^100+d^100)*(c + e + f)
R = ZZ[a..f, MonomialOrder=>RevLex, Global=>false];
a^2*(c+d) + b*(c^100+d^100)*(c + e + f)
