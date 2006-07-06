R = QQ[a..f];
g = matrix{{a,b},{c,d},{e,f}}
M = subquotient(g,matrix{{b},{c},{d}})
cover M
cover M == source generators M
