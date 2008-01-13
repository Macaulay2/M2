{1,2,3} === {1,2,3}
{1,2,3} === {2,1,3}
R = QQ[a..d];
a^2+b === b+a^2
ideal(a^2+b,c*d) === ideal(b+a^2,c*d+b+a^2)
matrix{{a,b,c}} === matrix{{a,b,c}}
matrix{{a,b,c}} === transpose matrix{{a},{b},{c}}
