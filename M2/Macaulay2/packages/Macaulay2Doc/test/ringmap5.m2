B = QQ[a..d];
use B
G = map(B,B,{a=>1, b=>b^4})
G1 = map(B,B,sub(vars B,{a=>1, b=>b^4}))
assert(G === G1)

