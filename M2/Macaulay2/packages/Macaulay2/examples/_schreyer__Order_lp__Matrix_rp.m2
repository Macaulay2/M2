R = ZZ/101[a..d];
m = matrix{{a,b,c,d}};
f = schreyerOrder m
g = syz f
leadTerm g
hf = map(source f, 1, {{d},{c},{b},{a}})
hm = map(source m, 1, {{d},{c},{b},{a}})        
leadTerm hf
leadTerm hm
schreyerOrder source m
schreyerOrder source f
