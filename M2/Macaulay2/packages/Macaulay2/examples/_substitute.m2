A = QQ[a..f]; B = QQ[a..d]; C = ZZ/101[x,y];
F = 3*a^2-b-d+101*c
sub(F, {a=>1, b=>b^4})
sub(F, matrix{{x,y,1,0}})
sub(F, A)
D = B/(a*b*c*d);
sub(F,D)
use ring F;
sub(F, {a=>1, b=>3, c=> 1, d=>13})
sub(1/3*a*b, {a=>1, b=>1, c=>1, d=>1})
sub(1/3*a*b, {a=>1_QQ, b=>1, c=>1, d=>1})       
use B;
M = image(vars B ++ vars B)
N = substitute(M, {a=>b+c,c=>1})
M' = prune M
N' = coker substitute(presentation M', {a=>b+c,c=>1})
hf = hilbertSeries coker matrix{{a,b^3,d^5}}
hf1 = reduceHilbert hf
use ring numerator hf;
sub(hf1, T => -1)
sub(hf, T => a)
value oo
oo == value sub(hf1, T=>a)
use B;
G = map(B,B,{a=>1, b=>b^4})
G F
