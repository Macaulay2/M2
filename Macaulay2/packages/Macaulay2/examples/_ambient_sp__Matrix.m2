R = QQ[a..d];
f = map(image vars R, coker matrix{{a,b},{c,d}}, transpose matrix{{a,b,c,d},{d,c,b,a}})
target f
source f
ambient f
