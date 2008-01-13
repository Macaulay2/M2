f = x -> x+1
g = x -> 2*x
g \ (1 .. 10)
(1 .. 10) / g
f \ g \ (1 .. 10)
f @@ g \ (1 .. 10)
set (1 .. 10)
g \ oo
R = QQ[x];
f = map(R,R,{x^2})
f \ {x,x^2,x^3,x^4}
