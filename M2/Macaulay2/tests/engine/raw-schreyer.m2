--status: this old test depends on internal things and probably should be deleted

---------------------------------------------------
-- Test of engine Schreyer order code -------------
---------------------------------------------------
-- Also tests whether these are connected at top level correctly
-- schreyerOrder calls rawFreeModule, rawGetSchreyer depending on 
--   its parameter.
needs "raw-util.m2"
errorDepth = 0

R = ZZ/101[symbol a..symbol d]
m = matrix{{a,b,c,d}}
F = R^{-1,-1,-1,-1}
F2 = schreyerOrder m
assert(not (F === F2))
f = schreyerOrder m
assert(schreyerOrder source f != 0)
g = schreyerOrder f

-- check that iterated schreyer orders are correct
errorDepth = 0
needs "raw-util.m2"
R = ZZ/101[symbol a..symbol d]
m = matrix{{b^2-a*c, c^2-b*d, b*c-a*d}}
m1 = syz m
raw target m1
raw source m1
ms = schreyerOrder m
raw source ms
ms2 = syz ms
raw source ms2
f2 = schreyerOrder ms2
raw f2
raw source f2

-- initial terms in Schreyer orders
leadTerm ms
leadTerm ms2
toString raw target ms2
h = gens gb ms2
toString target raw h

<< "FIX: syz should return Schreyer free modules??" << endl;

-- Local Variables:
-- compile-command: "M2 -e errorDepth=0 --stop -e 'load \"raw-schreyer.m2\"' -e 'exit 0' "
-- End:
