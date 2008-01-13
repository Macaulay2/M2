R = QQ[x,y,a..d,t,MonomialOrder=>{2,4,1}];
m = matrix{{x*a-d^2, a^3-1, x-a^100, a*b*d+t*c^3, t^3-t^2-t+1}}
selectInSubring(1,m)
selectInSubring(2,m)
S = QQ[a..d,MonomialOrder=>Lex];
m = matrix{{a^2-b, b^2-c, c^2-d, d^2-1}}
selectInSubring(1,m)
S = QQ[a..d,MonomialOrder=>{4:1}];
m = matrix{{a^2-b, b^2-c, c^2-d, d^2-1}}
selectInSubring(1,m)
selectInSubring(2,m)
selectInSubring(3,m)    
