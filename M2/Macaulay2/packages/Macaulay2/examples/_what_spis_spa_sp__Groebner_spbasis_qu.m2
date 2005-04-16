R = QQ[a..d,MonomialOrder=>GRevLex]
F = a^3 + d^2 + a*d + b*c + 1
R = QQ[a..d,MonomialOrder=>RevLex]
substitute(F,R)
R = QQ[a..d,MonomialOrder=>Lex]
substitute(F,R)
R = QQ[a..d,Weights=>{1,1,0,0}]
substitute(F,R)
R = QQ[a..d,Weights=>{-1,0,0,0}]
substitute(F,R)
R = QQ[a..d,Weights=>{-1,-1,-1,-1}]
substitute(F,R)
R = QQ[a..d,MonomialOrder=>ProductOrder{1,3}]
substitute(F,R)
leadTerm F
