R = QQ[a,b];
f1 = matrix{{a,b}}
f = a * f1
degree f
source f == source f1
g = map(f, Degree => 0)
degree g
source g == (source f) ** R^{-1}
g2 = a ** matrix{{a,b}}
degree g2
isHomogeneous g2
