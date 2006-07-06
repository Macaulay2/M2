R = QQ[a..d,MonomialOrder=>{Weights => {-1,2,3,4}},Global=>false];
f = a^2 + b+ c^2 + b*d
leadTerm f
leadTerm(1,ideal(f))
R = QQ[a..d,MonomialOrder=>{Weights => {1,2,3,4}, Weights => {2,4,2,1}}];
f = a^6 + b^3+ c^2
leadTerm(f)
leadTerm(1, ideal(f))
leadTerm(2, ideal(f))
leadTerm(3, ideal(f))
R = QQ[a..d,MonomialOrder=>{Weights => {1,2}, Lex}];
f = a^2 + b+ c^2 + b*d
