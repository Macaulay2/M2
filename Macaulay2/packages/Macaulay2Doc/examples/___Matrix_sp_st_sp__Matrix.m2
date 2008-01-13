R = QQ[a,b,c,x,y,z];
f = matrix{{x},{y},{z}}
g = matrix{{a,b,c}}
f*g
target (f*g) == target f
source (f*g) == source g
isHomogeneous (f*g)
degree(f*g)
h = map(f*g,Degree=>0)
degree h
degrees source h
