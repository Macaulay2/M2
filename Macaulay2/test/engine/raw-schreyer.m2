-- Test of the Schreyer order routines
errorDepth = 0
needs "raw-util.m2"
R = ZZ/101[a..d]
m = matrix{{a,b,c,d}}
F = R^{-1,-1,-1,-1}
F2 = schreyerOrder m
assert(not (F === F2))
f = map(target m, schreyerOrder m, m)
assert(schreyerOrder source f != 0)
g = schreyerOrder f
source f === schreyerOrder schreyerOrder g
F2 == source f

-- check that iterated schreyer orders are correct
errorDepth = 0
needs "raw-util.m2"
R = ZZ/101[a..d]
m = matrix{{b^2-a*c, c^2-b*d, b*c-a*d}}
m1 = syz m
raw target m1
raw source m1
ms = map(target m, schreyerOrder m, m)
raw source ms
ms2 = syz ms
raw source ms2
F2 = schreyerOrder ms2
raw F2

-- initial terms in Schreyer orders
leadTerm ms
leadTerm ms2
toString raw target ms2
h = gens gb ms2
toString target raw h

<< "FIX: syz should return Schreyer free modules??" << endl;
